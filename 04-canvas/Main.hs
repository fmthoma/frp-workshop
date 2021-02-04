module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = UI.startGUI UI.defaultConfig setup

setup :: Window -> UI ()
setup window = do
    btnIncrement <- UI.button # set UI.text "+"
    btnDecrement <- UI.button # set UI.text "-"
    elemCounter <- UI.new
    getBody window #+ [row [element btnDecrement, element elemCounter, element btnIncrement]]

    let eIncrement = increment <$ UI.click btnIncrement
        eDecrement = decrement <$ UI.click btnDecrement
        eCounter = unionWith (.) eIncrement eDecrement
    bCounter <- accumB 0 eCounter
    element elemCounter # sink UI.text (show <$> bCounter)
    pure ()

increment, decrement :: Int -> Int
increment = (+1)
decrement = subtract 1
