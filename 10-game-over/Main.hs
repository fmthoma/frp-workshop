{-# LANGUAGE RecursiveDo #-}
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Data.Foldable (for_)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import System.Random (mkStdGen, randomRs)

main :: IO ()
main = UI.startGUI UI.defaultConfig setup

{-- Model --}
data ApplicationState = ApplicationState
    { snake :: Snake
    , direction :: Direction
    , item :: Point
    , score :: Int
    , prng :: [Int] }
    deriving (Show)

data Point = Point Int Int
    deriving (Eq, Show)

-- First item is the head of the snake
newtype Snake = Snake { points :: [Point] }
    deriving (Show)

data Direction = L | U | R | D
    deriving (Show)

initialState :: ApplicationState
initialState = ApplicationState
    { snake = Snake [Point 4 0, Point 3 0, Point 2 0, Point 1 0, Point 0 0]
    , direction = R
    , item = Point x y
    , score = 0
    , prng = prng }
  where
    x : y : prng = randomRs (0, 19) (mkStdGen 42)

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

moveSnake :: Direction -> Snake -> Snake
moveSnake direction snake = Snake (move direction (head xs) : init xs)
  where xs = points snake

step :: ApplicationState -> ApplicationState
step state = state { snake = moveSnake (direction state) (snake state) }

changeDirection :: Direction -> ApplicationState -> ApplicationState
changeDirection newDirection state = state { direction = newDirection }

canLevelUp :: ApplicationState -> Bool
canLevelUp state = head (points (snake state)) == item state

levelUp :: ApplicationState -> ApplicationState
levelUp state = state { snake = snake', item = Point x' y', score = score', prng = prng' }
  where
    x' : y' : prng' = prng state
    ps = points (snake state)
    snake' = Snake { points = ps ++ [last ps]}
    score' = score state + 1

{-- View --}
data Page = Page
    { elemCanvas :: Element
    , btnReset :: Element
    , elemScore :: Element }

createPage :: Window -> UI Page
createPage window = do
    elemCanvas <- UI.canvas
        # set UI.style [("background", "grey")]
        # set UI.width 400
        # set UI.height 400
    btnReset <- UI.button # set UI.text "Reset"
    elemScore <- UI.new
    getBody window #+ [column [element elemCanvas, row [element elemScore, element btnReset]]]
    pure Page { elemCanvas = elemCanvas, btnReset = btnReset, elemScore = elemScore }

render :: Page -> ApplicationState -> UI ()
render page state = do
    resetCanvas page
    for_ (points (snake state)) $ \point ->
        drawPixel page (UI.htmlColor "black") point
    drawPixel page (UI.htmlColor "green") (item state)

drawPixel :: Page -> UI.FillStyle -> Point -> UI ()
drawPixel page color (Point x y) = do
    elemCanvas page # set' UI.fillStyle color
    elemCanvas page # UI.fillRect (20 * fromIntegral x, 20 * fromIntegral y) 20 20

resetCanvas :: Page -> UI ()
resetCanvas page =
    elemCanvas page # UI.clearCanvas

{-- Controller --}
setup :: Window -> UI ()
setup window = mdo
    page <- createPage window
    elemBody <- getBody window

    (eClock, tClock) <- liftIO newEvent
    liftIO $ forkIO $ forever $ do
        threadDelay 300000
        tClock ()

    let eStep = step <$ eClock
        eReset = const initialState <$ UI.click (btnReset page)
        eChangeDirection = changeDirection <$> filterJust (keyCodeToDirection <$> UI.keydown elemBody)
        -- Look ahead one step since we want to level up *when* capturing, not *after* capturing
        eLevelUp = levelUp <$ whenE (canLevelUp . step <$> bApp) eClock
        eApp = concatenate <$> unions [eStep, eReset, eChangeDirection, eLevelUp]
    bApp <- accumB initialState eApp

    element (elemScore page)
        # sink UI.text (("Score: " ++) . show . score <$> bApp)

    onChanges bApp $ render page

keyCodeToDirection :: UI.KeyCode -> Maybe Direction
keyCodeToDirection keyCode = case keyCode of
    37 -> Just L
    38 -> Just U
    39 -> Just R
    40 -> Just D
    _  -> Nothing
