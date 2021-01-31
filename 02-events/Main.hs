module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = UI.startGUI UI.defaultConfig setup

setup :: Window -> UI ()
setup window = do
    div <- UI.new # set UI.text "Hello World"
    getBody window #+ [element div]
    pure ()
