module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = UI.startGUI UI.defaultConfig setup

{-- Model --}
data ApplicationState = ApplicationState
    { location :: Point }
    deriving (Show)

data Point = Point Int Int
    deriving (Show)

initialPoint :: Point
initialPoint = Point 0 0

moveLeft, moveUp, moveRight, moveDown :: Point -> Point
moveLeft  (Point x y) = Point (x-1) y
moveUp    (Point x y) = Point x (y-1)
moveRight (Point x y) = Point (x+1) y
moveDown  (Point x y) = Point x (y+1)

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

render :: Page -> ApplicationState -> UI ()
render page (ApplicationState { location = location }) = do
    canvas page # UI.clearCanvas
    drawBlackPixel page location

drawBlackPixel :: Page -> Point -> UI ()
drawBlackPixel page (Point x y) = do
    canvas page # set' UI.fillStyle (UI.htmlColor "black")
    canvas page # UI.fillRect (20 * fromIntegral x, 20 * fromIntegral y) 20 20

resetCanvas :: Page -> UI ()
resetCanvas (Page { canvas = canvas }) = canvas # UI.clearCanvas

{-- Controller --}
setup :: Window -> UI ()
setup window = do
    page <- createPage window
    body <- getBody window

    let eMotion = filterJust $ arrowKeyToMotion <$> UI.keydown body
        eResetPoint = const initialPoint <$ UI.click (resetButton page)
        ePoint = unionWith (.) eMotion eResetPoint
    bPoint <- accumB initialPoint ePoint

    let bApplication = ApplicationState <$> bPoint

    onChanges bApplication $ render page
    pure ()

arrowKeyToMotion :: UI.KeyCode -> Maybe (Point -> Point)
arrowKeyToMotion keyCode = case keyCode of
    37 -> Just moveLeft
    38 -> Just moveUp
    39 -> Just moveRight
    40 -> Just moveDown
    _  -> Nothing
