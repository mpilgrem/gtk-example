{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

{-|
Based on the example 'Hello World' program in @README.md@ at the
@haskell-gi/haskell-gi@ github repository.
-}
module Main
  (
    main
  ) where

-- package gi-gtk
import GI.Gtk (AttrOp ((:=)), new, on, set)
import GI.Gtk.Functions (mainQuit)
import qualified GI.Gtk.Functions as Gtk (init, main)
import GI.Gtk.Objects.Button (Button (Button))
import GI.Gtk.Objects.Window (Window (Window))

main :: IO ()
main = do
  Gtk.init Nothing
  win <- new Window [ #title := "Hi there" ]
  on win #destroy mainQuit
  button <- new Button [ #label := "Click me" ]
  on button #clicked (set button [ #sensitive := False,
                                   #label := "Thanks for clicking me" ])
  #add win button
  #showAll win
  Gtk.main
