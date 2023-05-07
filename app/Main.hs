{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import qualified Data.Text as T

import qualified GI.Gtk as Gtk

import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore (MSF(..))
import FRP.BearRiver

-- from this package
import FRP.Dynamic
import FRP.Gtk.Base
import FRP.Gtk.Containers
import FRP.Gtk.Widgets


--------------------------
-- helper functions

eitherE :: Monad m => SF m (Event a, Event b) (Event (Either a b))
eitherE = arr $ \(e1, e2) -> lMerge (Left <$> e1) (Right <$> e2)

accumBy1 :: forall m a. Monad m => (a -> a) -> SF m (Event a, Event ()) (Event a)
accumBy1 f = eitherE >>> accumBy f' NoEvent >>> arr join
  where
    f' :: Event a -> Either a () -> Event a
    f' NoEvent (Left a) = Event a
    f' (Event _) (Left a) = Event a
    f' NoEvent (Right ()) = NoEvent
    f' (Event a) (Right ()) = Event (f a)

switchList :: Monad m => SF m (Event [SF m () b]) [b]
switchList = switchList' []

switchList' :: Monad m => [SF m () b] -> SF m (Event [SF m () b]) [b]
switchList' c = switch ((parB c <<< constant ()) &&& (noEvent --> identity)) switchList'

    
-- | A ‘hybrid’ switching combinator halfway between 'pSwitchB' and
-- 'dpSwitchB'. When a switch event is received, the new SF is used at
-- the time of switching (like 'pSwitchB'), but only accepts input
-- 'Event's at later times (like 'dpSwitchB') to avoid infinite loops.
hpSwitchB
    :: (Monad m, Traversable col)
    => col (SF m (Event a) b)
    -> SF m (Event a, col b) (Event c)
    -> (col (SF m (Event a) b) -> c -> SF m (Event a) (Dynamic (col b)))
    -> SF m (Event a) (Dynamic (col b))
hpSwitchB sfs sfF sfCs = MSF $ \a -> do
    res <- mapM (`unMSF` a) sfs
    let bs   = fmap fst res
        sfs' = fmap snd res
    (e,sfF') <- unMSF sfF (a, bs)
    case e of
        Event c -> do
            let sfsSwitched = sfCs sfs' c
            (bsSwitched, sfs'Switched) <- unMSF sfsSwitched NoEvent
            pure (bsSwitched {modified=True}, sfs'Switched)
        NoEvent -> pure (Dynamic False bs, hpSwitchB sfs' sfF' sfCs)

dpSwitchB'
    :: (Monad m , Traversable col)
    => col (SF m a b)
    -> SF m (a, col b) (Event c)
    -> (col (SF m a b) -> c -> SF m a (col b))
    -> SF m a (col b)
dpSwitchB' sfs sfF sfCs = MSF $ \a -> do
  res <- mapM (`unMSF` a) sfs
  let bs   = fmap fst res
      sfs' = fmap snd res
  (e,sfF') <- unMSF sfF (a, bs)
  let ct = case e of
          Event c -> sfCs sfs' c
          NoEvent -> dpSwitchB' sfs' sfF' sfCs
  return (bs, ct)


--------------------------
-- sample GUIs

-- A simple counter with 'up' and 'down' buttons
counterExample :: SF UI () (Dynamic Gtk.Box)
counterExample = proc () -> do
    (up, upBtn) <- button "Up" -< ()
    (down, downBtn) <- button "Down" -< ()

    value <- accumBy (flip ($)) (0::Int) -< lMerge ((+1) <$ up) (subtract 1 <$ down)
        
    valueDisplay <- entry -< T.pack . show <$> value

    btnBox <- box Gtk.OrientationVertical -< updated $ sequenceA
        [ (,True,True,0) . Some <$> upBtn
        , (,True,True,0) . Some <$> downBtn
        ]
    box Gtk.OrientationHorizontal -< updated $ sequenceA
        [ (,False,False,0) . Some <$> entryWidget valueDisplay
        , (,True ,True ,0) . Some <$> btnBox
        ]
    
-- Two buttons which swap positions when clicked
example1 :: SF UI () (Dynamic Gtk.Box)
example1 = proc () -> do
    (e1, btn1) <- button "Click me!" -< ()
    (e2, btn2) <- button "Click me too!" -< ()
    swappedBtns <- accumBy1 swap -< (updated $ sequenceA [btn1, btn2], lMerge e1 e2)
    box Gtk.OrientationHorizontal -< (fmap.fmap) (\btn -> (Some btn, True, True, 0)) swappedBtns
  where
    swap [a,b] = [b,a]
    swap _ = error "swap: expected two widgets only!"

-- Two text entries, the second showing the reverse of the first,
-- with a label below showing the text last typed into the first entry
example2 :: SF UI () (Dynamic Gtk.Box)
example2 = proc () -> do
    n <- never -< ()
    e1 <- entry -< n
    e2 <- entry -< T.reverse <$> updated (entryText e1)
    insertedText <- hold "" -< T.pack . show <$> entryInserted e1
    l <- label -< insertedText
    pane <- hpaned -< updated $ (,) <$> entryWidget e1 <*> entryWidget e2
    box Gtk.OrientationVertical -< updated $ sequenceA
        [ (,True,False,0) . Some <$> pane
        , (,True,False,0) . Some <$> l
        ]

data CounterEvent = AddCounter | RemoveCounter Int
    deriving (Show)

-- Dynamic creation/destruction of widgets. Uses 'counterExample'.
example3 :: SF UI () (Dynamic Gtk.Box)
example3 = proc () -> do
    (addEv, addBtn) <- button "+" -< ()
    counterWidgets <- arr mkList <<< go [] -< addEv
    box Gtk.OrientationHorizontal -< updated $
        snoc <$> (fmap ((,False,False,30) . Some) <$> counterWidgets)
             <*> fmap ((,True,True,0) . Some) addBtn
  where
    go :: [SF UI (Event ()) (Event (), Dynamic Gtk.Box)] -> SF UI (Event ()) (Dynamic [(Event (), Dynamic Gtk.Box)])
    go curState = hpSwitchB curState route nextState

    route :: SF UI (Event (), [(Event (), Dynamic Gtk.Box)]) (Event CounterEvent)
    route = arr $ \(addEv, cs) -> mergeEvents $
           (AddCounter <$ addEv) :
           zipWith (\i (remEv, _) -> RemoveCounter i <$ remEv) [0..] cs

    nextState :: [SF UI (Event ()) (Event (), Dynamic Gtk.Box)] -> CounterEvent -> SF UI (Event ()) (Dynamic [(Event (), Dynamic Gtk.Box)])
    nextState s AddCounter = go (s ++ [removableCounter <<< constant ()])
    nextState s (RemoveCounter i) = go $ removeAt i s

    mkList :: Dynamic [(e, Dynamic b)] -> Dynamic [b]
    mkList d = d >>= traverse snd

    removeAt 0 (_:xs) = xs
    removeAt n (x:xs) = x : removeAt (n-1) xs
    removeAt _ [] = []

    snoc xs x = xs ++ [x]

    removableCounter = proc () -> do
        counter <- counterExample -< ()
        (removeEv, removeBtn) <- button "-" -< ()
        cbox <- box Gtk.OrientationVertical -< updated $ sequenceA
            [ (,False,False,0) . Some <$> counter
            , (,False,False,0) . Some <$> removeBtn
            ]
        identity -< (removeEv, cbox)

main :: IO ()
main = runGui $ arr updated <<< example3
