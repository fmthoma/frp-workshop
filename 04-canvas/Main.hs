module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = UI.startGUI UI.defaultConfig setup

setup :: Window -> UI ()
setup window = do
    incrementButton <- UI.button # set UI.text "+"
    decrementButton <- UI.button # set UI.text "-"
    display <- UI.div
    getBody window #+ [row [element decrementButton, element display, element incrementButton]]

    let eIncrement = increment <$ UI.click incrementButton
        eDecrement = decrement <$ UI.click decrementButton
        eCounter = unionWith (.) eIncrement eDecrement
    bCounter <- accumB 0 eCounter
    element display # sink UI.text (show <$> bCounter)
    pure ()

increment, decrement :: Int -> Int
increment = (+1)
decrement = subtract 1
