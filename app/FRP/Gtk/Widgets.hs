{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module FRP.Gtk.Widgets where

import Control.Monad
import qualified Data.Dynamic as D
import Data.Text (Text)
import Data.Word (Word32)

import Data.GI.Base hiding (get)
import qualified GI.Gtk as Gtk

import Control.Monad.Trans.MSF
import Data.MonadicStreamFunction
import FRP.BearRiver

import FRP.Dynamic
import FRP.Gtk.Base

button :: Text -> SF UI () (Event (), Dynamic Gtk.Button)
button l = performOnFirstSample $ do
    buttonW <- new Gtk.Button [ #label := l ]
    m <- trigger (\h -> void $ on buttonW #clicked $ h $ D.toDyn ())
    pure $ (,) <$> m <*> constDyn buttonW

data Entry = Entry
    { entryText :: Dynamic Text
    , entryInserted :: Event Text
    , entryWidget :: Dynamic Gtk.Entry
    }

entry :: SF UI (Event Text) Entry
entry = performOnFirstSample $ do
    entryW <- new Gtk.Entry []
    buf <- #getBuffer entryW
    -- make sure to rerun network on change!
    ei <- trigger @(Word32,Text,Word32) $ \h -> void $ on buf #insertedText $ \j k l -> h $ D.toDyn (j,k,l)
    ed <- trigger @(Word32,     Word32) $ \h -> void $ on buf #deletedText $ \j k -> h $ D.toDyn (j,k)
    let t = \case
            NoEvent -> #getText entryW
            Event t' -> t' <$ #setText entryW t'
    pure $ proc e -> do
        text <- arrM t -< e
        insertion <- ei -< ()
        deletion <- ed -< ()
        let changed = lMerge (void insertion) (void deletion)
        changedText <- holdDyn "" -< tagWith text changed
        widget <- constDyn entryW -< ()
        identity -< Entry
            { entryText = changedText
            , entryInserted = med <$> insertion
            , entryWidget = widget
            }
  where
    med (_,x,_) = x

label :: SF UI Text (Dynamic Gtk.Label)
label = performOnFirstSample $ do
    labelW <- new Gtk.Label []
    pure $ arrM (#setText labelW) >>> constDyn labelW
