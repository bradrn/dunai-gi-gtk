# dunai-gi-gtk

This is an illustration of how to integrate a GUI library (in this case `gi-gtk`)
  into [`dunai`](https://hackage.haskell.org/package/dunai),
  using [`bearriver`](https://hackage.haskell.org/package/bearriver) for arrowised FRP.
Though still at a prototype stage it already provides a very convenient way to write programs,
  e.g. the following simple counter:


```hs
main :: IO ()
main = runGUI $ proc () -> do
    -- set up buttons
    (up, upBtn)     <- button "Up"   -< ()
    (down, downBtn) <- button "Down" -< ()

    -- compute value and display it
    value <- accumBy (flip ($)) (0::Int) -< lMerge ((+1) <$ up) (subtract 1 <$ down)
    valueDisplay <- entry -< T.pack . show <$> value

    -- layout widgets in boxes
    btnBox <- box Gtk.OrientationVertical -< updated $ sequenceA
        [ (,True,True,0) . Some <$> upBtn
        , (,True,True,0) . Some <$> downBtn
        ]
    windowBox <- box Gtk.OrientationHorizontal -< updated $ sequenceA
        [ (,False,False,0) . Some <$> entryWidget valueDisplay
        , (,True ,True ,0) . Some <$> btnBox
        ]
    arr updated -< windowBox
```
