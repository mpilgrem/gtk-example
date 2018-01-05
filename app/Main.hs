{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

{-|
Based on the example 'Hello World' program in @README.md@ at the
@haskell-gi/haskell-gi@ github repository.
-}
module Main
  (
    main
  ) where

import System.Environment (getArgs)

-- package gi-gtk
import GI.Gtk (AttrOp ((:=)), Button (Button), buttonLabel, new, on,
  onButtonClicked, onWidgetDestroy, set, widgetSensitive, windowTitle)
import GI.Gtk.Functions (mainQuit)
import qualified GI.Gtk.Functions as Gtk (init, main)
import GI.Gtk.Objects.Window (Window (Window))

main :: IO ()
main = getArgs >>= pickMain
 where
  pickMain args
    | null args || args!!0 /= "-o"
    = main'
    | otherwise
    = main''

-- Using overloaded labels
main' :: IO ()
main' = do
  Gtk.init Nothing
  win <- new Window [ #title := "Hi there" ]
  on win #destroy mainQuit
  button <- new Button [ #label := "Click me" ]
  on button #clicked (set button [ #sensitive := False,
                                   #label := "Thanks for clicking me" ])
  #add win button
  #showAll win
  Gtk.main

-- Notusing overloaded labels
main'' :: IO ()
main'' = do
  Gtk.init Nothing
  win <- new Window [ windowTitle := "Hi there" ]
  onWidgetDestroy win mainQuit
  button <- new Button [ buttonLabel := "Click me" ]
  onButtonClicked button (set button [ widgetSensitive := False,
                                       buttonLabel := "Thanks for clicking me" ])
  #add win button
  #showAll win
  Gtk.main
