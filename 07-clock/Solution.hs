module Solution where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = UI.startGUI UI.defaultConfig setup

{-- Model --}
data ApplicationState = ApplicationState
    { location :: Point
    , direction :: Direction }
    deriving (Show)

data Point = Point Int Int
    deriving (Show)

data Direction = L | U | R | D
    deriving (Show)

initialPoint :: Point
initialPoint = Point 0 0

initialDirection :: Direction
initialDirection = R

initialState :: ApplicationState
initialState = ApplicationState { location = initialPoint, direction = initialDirection }

moveLeft, moveUp, moveRight, moveDown :: Point -> Point
moveLeft  (Point x y) = Point (x-1) y
moveUp    (Point x y) = Point x (y-1)
moveRight (Point x y) = Point (x+1) y
moveDown  (Point x y) = Point x (y+1)

move :: Direction -> Point -> Point
move direction = case direction of
    L -> moveLeft
    U -> moveUp
    R -> moveRight
    D -> moveDown

step :: ApplicationState -> ApplicationState
step state = state { location = move (direction state) (location state) }

changeDirection :: Direction -> ApplicationState -> ApplicationState
changeDirection newDirection state = state { direction = newDirection }

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

    (eClock, tClock) <- liftIO newEvent
    liftIO $ forkIO $ forever $ do
        threadDelay 300000
        tClock ()

    let eStep, eReset, eChangeDirection :: Event (ApplicationState -> ApplicationState)
        eStep = step <$ eClock
        eReset = const initialState <$ UI.click (resetButton page)
        eChangeDirection = changeDirection <$> filterJust (arrowKeyToDirection <$> UI.keydown body)
        eApp = concatenate <$> unions [eStep, eReset, eChangeDirection]
    bApp <- accumB initialState eApp

    onChanges bApp $ render page
    pure ()

arrowKeyToDirection :: UI.KeyCode -> Maybe Direction
arrowKeyToDirection keyCode = case keyCode of
    37 -> Just L
    38 -> Just U
    39 -> Just R
    40 -> Just D
    _  -> Nothing
