module Solution where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = UI.startGUI UI.defaultConfig setup

{-- Model --}
data ApplicationState = ApplicationState
    { position :: Point }
    deriving (Show)

data Point = Point Int Int
    deriving (Show)

data Direction = L | U | R | D
    deriving (Show)

initialState :: ApplicationState
initialState = ApplicationState
    { position = Point 0 0 }

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

mapPosition :: (Point -> Point) -> ApplicationState -> ApplicationState
mapPosition f state = state { position = f (position state) }

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

render :: Page -> ApplicationState -> UI ()
render page state = do
    resetCanvas page
    drawBlackPixel page (position state)

drawBlackPixel :: Page -> Point -> UI ()
drawBlackPixel page (Point x y) = do
    elemCanvas page # set' UI.fillStyle (UI.htmlColor "black")
    elemCanvas page # UI.fillRect (20 * fromIntegral x, 20 * fromIntegral y) 20 20

resetCanvas :: Page -> UI ()
resetCanvas page =
    elemCanvas page # UI.clearCanvas

{-- Controller --}
setup :: Window -> UI ()
setup window = do
    page <- createPage window
    elemBody <- getBody window

    let eMove = mapPosition . move <$> filterJust (keyCodeToDirection <$> UI.keydown elemBody)
        eReset = const initialState <$ UI.click (btnReset page)
        eApp = unionWith (.) eMove eReset
    bApp <- accumB initialState eApp

    onChanges bApp $ render page

keyCodeToDirection :: UI.KeyCode -> Maybe Direction
keyCodeToDirection keyCode = case keyCode of
    37 -> Just L
    38 -> Just U
    39 -> Just R
    40 -> Just D
    _  -> Nothing
