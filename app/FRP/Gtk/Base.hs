{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module FRP.Gtk.Base
       ( UI
       , trigger
       , runGui
       ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.Dynamic as D
import Data.Foldable (for_)
import Data.Unique

import Data.GI.Base hiding (get)
import GI.Gdk (threadsAddIdle)
import GI.GLib.Constants (pattern PRIORITY_DEFAULT)
import qualified GI.Gtk as Gtk

import Control.Monad.Trans.MSF
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore (MSF(..))
import FRP.BearRiver

data DownUp = DownUp
    { down :: Maybe (Unique, D.Dynamic)
    , up :: [(Unique, (D.Dynamic -> IO ()) -> IO ())]
    }
type UI = StateT DownUp IO
    
trigger
    :: forall a.
       D.Typeable a
    => ((D.Dynamic -> IO ()) -> IO ()) -> ReaderT DTime UI (SF UI () (Event a))
trigger h = do
    u <- liftIO newUnique
    let m = readerS $ stateS $ arr $ \(DownUp{down,up}, (_, ())) ->
            case down of
                Just (u',e) | u==u' -> 
                    let down' = (u',error "tried to access triggered event twice within an MSF!")
                    in
                    ( DownUp (Just down') up
                    , Event $ D.fromDyn @a e (error "dynamic type mismatch")
                    )
                _ -> (DownUp down up, NoEvent)
    lift $ modify $ \du -> du { up = (u,h) : up du }
    pure m

runGui :: Gtk.IsWidget w => SF UI () (Event w) -> IO ()
runGui m = do
    _ <- Gtk.init Nothing

    win <- new Gtk.Window []
    _ <- on win #destroy Gtk.mainQuit

    events <- newChan

    ((w, m'), du) <-
        flip runStateT (DownUp Nothing []) $
        flip runReaderT 0 $
            reactimateE m
    refreshGuiState win events (Event w) du

    void . forkIO $ do
        runLoop win events m'
        Gtk.mainQuit

    #showAll win
    Gtk.main

runLoop :: Gtk.IsWidget w => Gtk.Window -> Chan (Unique, D.Dynamic) -> SF UI () (Event w) -> IO ()
runLoop win events m = do
    ev <- readChan events

    m' <- runUI $ do
        ((w, m'), du) <-
            flip runStateT (DownUp (Just ev) []) $
            flip runReaderT 0 $
                unMSF m ()
        refreshGuiState win events w du
        pure m'

    runLoop win events m'
  where
    -- from gi-gtk-declarative-simple
    runUI :: IO a -> IO a
    runUI ma = do
        r <- newEmptyMVar
        runUI_ (ma >>= putMVar r)
        takeMVar r

    runUI_ :: IO () -> IO ()
    runUI_ ma = void . threadsAddIdle PRIORITY_DEFAULT $ do
        ma
        return False

reactimateE :: Monad m => MSF m () (Event a) -> m (a, MSF m () (Event a))
reactimateE m = unMSF m () >>= \case
    (NoEvent, m') -> reactimateE m'
    (Event a, m') -> pure (a, m')

refreshGuiState :: Gtk.IsWidget w => Gtk.Window -> Chan (Unique, D.Dynamic) -> Event w -> DownUp -> IO ()
refreshGuiState win events w du = do
    -- add main widget if changed
    case w of
        Event widg -> #add win widg
        NoEvent -> pure ()

    -- attach any new signal handlers
    for_ (up du) $ \(u,h) -> h $ \dyn -> writeChan events (u,dyn)
