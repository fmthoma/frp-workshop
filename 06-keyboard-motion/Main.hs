module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = UI.startGUI UI.defaultConfig setup

{-- Model --}
data ApplicationState = ApplicationState
    { drawing :: DrawingState }

data DrawingState = Drawing | NotDrawing

isDrawing :: ApplicationState -> Bool
isDrawing state = case drawing state of
    Drawing -> True
    NotDrawing -> False

{-- View --}
data Page = Page
    { canvas :: Element
    , resetButton :: Element }

createPage :: Window -> UI Page
createPage window = do
    canvas <- UI.canvas
        # set UI.style [("background", "grey")]
        # set UI.width 400
        # set UI.height 400
    resetButton <- UI.button # set UI.text "Reset"
    getBody window #+ [element canvas, element resetButton]
    pure Page { canvas = canvas, resetButton = resetButton }

drawBlackPixel :: Page -> (Double, Double) -> UI ()
drawBlackPixel (Page { canvas = canvas }) (x, y) = do
    let pixelX = floor (x/20)
        pixelY = floor (y/20)
    canvas # set' UI.fillStyle (UI.htmlColor "black")
    canvas # UI.fillRect (20 * fromIntegral pixelX, 20 * fromIntegral pixelY) 20 20

resetCanvas :: Page -> UI ()
resetCanvas (Page { canvas = canvas }) = canvas # UI.clearCanvas

{-- Controller --}
setup :: Window -> UI ()
setup window = do
    page <- createPage window

    on UI.mousedown (canvas page) $ drawBlackPixel page

    on UI.click (resetButton page) $ \() -> resetCanvas page

    let eStartDrawing = Drawing    <$ UI.mousedown (canvas page)
        eStopDrawing  = NotDrawing <$ UI.mouseup   (canvas page)
        eDrawing = unionWith const eStartDrawing eStopDrawing
    bDrawing <- stepper NotDrawing eDrawing

    let bApplication = ApplicationState <$> bDrawing

    onEvent (whenE (isDrawing <$> bApplication) (UI.mousemove (canvas page))) $ drawBlackPixel page

    pure ()
