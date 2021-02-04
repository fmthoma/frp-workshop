module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = UI.startGUI UI.defaultConfig setup

{-- Model --}
data ApplicationState = ApplicationState
    { drawing :: DrawingState }

data DrawingState = Drawing | NotDrawing

initialState :: ApplicationState
initialState = ApplicationState { drawing = NotDrawing }

isDrawing :: ApplicationState -> Bool
isDrawing state = case drawing state of
    Drawing    -> True
    NotDrawing -> False

startDrawing :: ApplicationState -> ApplicationState
startDrawing state = state { drawing = Drawing }

stopDrawing :: ApplicationState -> ApplicationState
stopDrawing state = state { drawing = NotDrawing }

{-- View --}
data Page = Page
    { elemCanvas :: Element
    , btnReset :: Element }

createPage :: Window -> UI Page
createPage window = do
    elemCanvas <- UI.canvas
        # set UI.style [("background", "grey")]
        # set UI.width 400
        # set UI.height 400
    btnReset <- UI.button # set UI.text "Reset"
    getBody window #+ [element elemCanvas, element btnReset]
    pure Page { elemCanvas = elemCanvas, btnReset = btnReset }

drawBlackPixel :: Page -> (Int, Int) -> UI ()
drawBlackPixel page (x, y) = do
    elemCanvas page # set' UI.fillStyle (UI.htmlColor "black")
    elemCanvas page # UI.fillRect (20 * fromIntegral x, 20 * fromIntegral y) 20 20

resetCanvas :: Page -> UI ()
resetCanvas page =
    elemCanvas page # UI.clearCanvas

{-- Controller --}
setup :: Window -> UI ()
setup window = do
    page <- createPage window

    on UI.mousedown (elemCanvas page) $ \(x, y) ->
        drawBlackPixel page (floor (x / 20), floor (y / 20))

    on UI.click (btnReset page) $ \() -> resetCanvas page

    let eStartDrawing = startDrawing <$ UI.mousedown (elemCanvas page)
        eStopDrawing  = stopDrawing  <$ UI.mouseup   (elemCanvas page)
        eApp = unionWith (.) eStartDrawing eStopDrawing
    bApp <- accumB initialState eApp

    onEvent (whenE (isDrawing <$> bApp) (UI.mousemove (elemCanvas page))) $ \(x, y) ->
        drawBlackPixel page (floor (x / 20), floor (y / 20))

    pure ()
