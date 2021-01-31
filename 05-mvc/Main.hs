module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = UI.startGUI UI.defaultConfig setup

setup :: Window -> UI ()
setup window = do
    canvas <- UI.canvas
        # set UI.style [("background", "grey")]
        # set UI.width 400
        # set UI.height 400
    resetButton <- UI.button # set UI.text "Reset"
    getBody window #+ [element canvas, element resetButton]

    on UI.mousedown canvas $ \(x, y) ->
        drawBlackPixel canvas (floor (x / 20), floor (y / 20))

    on UI.click resetButton $ \() ->
        canvas # UI.clearCanvas

    let eMouseDown = True  <$ UI.mousedown canvas
        eMouseUp   = False <$ UI.mouseup   canvas
        eDrawing = unionWith const eMouseDown eMouseUp
    bDrawing <- stepper False eDrawing

    onEvent (whenE bDrawing (UI.mousemove canvas)) $ \(x, y) ->
        drawBlackPixel canvas (floor (x / 20), floor (y / 20))
    pure ()

drawBlackPixel :: Element -> (Int, Int) -> UI ()
drawBlackPixel canvas (x, y) = do
    canvas # set' UI.fillStyle (UI.htmlColor "black")
    canvas # UI.fillRect (20 * fromIntegral x, 20 * fromIntegral y) 20 20
