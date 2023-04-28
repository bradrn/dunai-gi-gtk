{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}

module FRP.Gtk.Containers
       ( hpaned
       , Some(..)
       , box
       ) where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Word (Word32)

import Data.GI.Base hiding (get)
import qualified GI.Gtk as Gtk

import Control.Monad.Trans.MSF
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore (MSF(..))
import FRP.BearRiver

import FRP.Dynamic
import FRP.Gtk.Base

unparent :: Gtk.IsWidget w => w -> IO ()
unparent w = Gtk.widgetGetParent w >>= \case
    Just c -> castTo Gtk.Container c >>= \case
        Just c' -> #remove c' w
        Nothing -> pure ()
    Nothing -> pure ()
    
hpaned :: (Gtk.IsWidget w1, Gtk.IsWidget w2) => SF UI (Event (w1, w2)) (Dynamic Gtk.Paned)
hpaned = performOnFirstSample $ do
    p <- new Gtk.Paned [ #orientation := Gtk.OrientationHorizontal ]
    pure $ nowDyn $ arrM $ \case
        NoEvent -> pure $ Dynamic False p
        Event (w1, w2) -> liftIO $ do
            unparent w1
            unparent w2
            #add1 p w1
            #add2 p w2
            pure $ Dynamic False p

nowDyn :: Monad m => MSF m a (Dynamic b) -> MSF m a (Dynamic b)
nowDyn m = MSF $ \a -> do
    (Dynamic _ b, m') <- unMSF m a
    pure (Dynamic True b, m')

data Some c = forall w. c w => Some w

box :: Gtk.Orientation -> SF UI (Event [(Some Gtk.IsWidget, Bool, Bool, Word32)]) (Dynamic Gtk.Box)
box o = performOnFirstSample $ do
    b <- new Gtk.Box [ #orientation := o ]
    pure $ nowDyn $ arrM $ \case
        NoEvent -> pure $ Dynamic False b
        Event ws -> liftIO $ do
            #foreach b $ \w -> #remove b w
            for_ ws $ \(Some w', expand, fill, pad) -> do
                w <- Gtk.toWidget w'
                unparent w
                #packStart b w expand fill pad
            #showAll b
            pure $ Dynamic False b
