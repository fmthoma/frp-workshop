module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = UI.startGUI UI.defaultConfig setup

setup :: Window -> UI ()
setup window = do
    elemCanvas <- UI.canvas
        # set UI.style [("background", "grey")]
        # set UI.width 400
        # set UI.height 400
    btnReset <- UI.button # set UI.text "Reset"
    getBody window #+ [element elemCanvas, element btnReset]

    on UI.mousedown elemCanvas $ \(x, y) ->
        drawBlackPixel elemCanvas (floor (x / 20), floor (y / 20))

    let eStartDrawing = const True  <$ UI.mousedown elemCanvas
        eStopDrawing  = const False <$ UI.mouseup   elemCanvas
        eDrawing = unionWith (.) eStartDrawing eStopDrawing
    bDrawing <- accumB False eDrawing

    onEvent (whenE bDrawing (UI.mousemove elemCanvas)) $ \(x, y) ->
        drawBlackPixel elemCanvas (floor (x / 20), floor (y / 20))

    on UI.click btnReset $ \() ->
        elemCanvas # UI.clearCanvas

drawBlackPixel :: Element -> (Int, Int) -> UI ()
drawBlackPixel elemCanvas (x, y) = do
    elemCanvas # set' UI.fillStyle (UI.htmlColor "black")
    elemCanvas # UI.fillRect (20 * fromIntegral x, 20 * fromIntegral y) 20 20
