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
newtype Snake = Snake [Point]
    deriving (Eq, Show)

data Direction = L | U | R | D
    deriving (Show)

initialSnake :: Snake
initialSnake = Snake [Point 4 0, Point 3 0, Point 2 0, Point 1 0, Point 0 0]

initialDirection :: Direction
initialDirection = R

initialState :: ApplicationState
initialState = ApplicationState
    { snake = initialSnake
    , direction = initialDirection
    , item = Point 15 10
    , score = 0
    , prng = randomRs (0, 19) (mkStdGen 42) }

moveLeft, moveUp, moveRight, moveDown :: Snake -> Snake
moveLeft  (Snake points@(Point x y : _)) = Snake (Point (x-1) y : init points)
moveUp    (Snake points@(Point x y : _)) = Snake (Point x (y-1) : init points)
moveRight (Snake points@(Point x y : _)) = Snake (Point (x+1) y : init points)
moveDown  (Snake points@(Point x y : _)) = Snake (Point x (y+1) : init points)

move :: Direction -> Snake -> Snake
move direction = case direction of
    L -> moveLeft
    U -> moveUp
    R -> moveRight
    D -> moveDown

step :: ApplicationState -> ApplicationState
step state = state { snake = move (direction state) (snake state) }

changeDirection :: Direction -> ApplicationState -> ApplicationState
changeDirection newDirection state = state { direction = newDirection }

isAlive :: ApplicationState -> Bool
isAlive ApplicationState { snake = Snake (head@(Point x y) : tail) } = not wallCollision && not snakeCollision
  where
    snakeCollision = head `elem` tail
    wallCollision = x < 0 || x >= 20 || y < 0 || y >= 20

doesCapture :: ApplicationState -> Bool
doesCapture ApplicationState { snake = Snake (hd:_), item = item } = hd == item

levelup :: ApplicationState -> ApplicationState
levelup state@(ApplicationState { snake = Snake s, item = Point x y, score = score, prng = prng }) =
    let x':y':prng' = prng
    in state
        { snake = Snake (s ++ [last s])
        , item = Point x' y'
        , score = score + 1
        , prng = prng' }

{-- View --}
data Page = Page
    { canvas :: Element
    , resetButton :: Element
    , scoreDisplay :: Element }

createPage :: Window -> UI Page
createPage window = do
    canvas <- UI.canvas
        # set UI.style [("background", "grey")]
        # set UI.width 400
        # set UI.height 400
    resetButton <- UI.button # set UI.text "Reset"
    scoreDisplay <- UI.new
    getBody window #+ [column [element canvas, row [element scoreDisplay, element resetButton]]]
    pure Page { canvas = canvas, resetButton = resetButton, scoreDisplay = scoreDisplay }

render :: Page -> ApplicationState -> UI ()
render page state@(ApplicationState { snake = Snake points, item = item }) = do
    let snakeColor = if isAlive state then "black" else "red"
    canvas page # UI.clearCanvas
    for_ points $ \point ->
        drawPixel page (UI.htmlColor snakeColor) point
    drawPixel page (UI.htmlColor "green") item

drawPixel :: Page -> UI.FillStyle -> Point -> UI ()
drawPixel page color (Point x y) = do
    canvas page # set' UI.fillStyle color
    canvas page # UI.fillRect (20 * fromIntegral x, 20 * fromIntegral y) 20 20

resetCanvas :: Page -> UI ()
resetCanvas (Page { canvas = canvas }) = canvas # UI.clearCanvas

{-- Controller --}
setup :: Window -> UI ()
setup window = mdo
    page <- createPage window
    body <- getBody window

    (eClock, tClock) <- liftIO newEvent
    liftIO $ forkIO $ forever $ do
        threadDelay 300000
        tClock ()

    let eStep, eReset, eChangeDirection :: Event (ApplicationState -> ApplicationState)
        eStep = whenE (isAlive <$> bApp) $ step <$ eClock
        eReset = const initialState <$ UI.click (resetButton page)
        eChangeDirection = changeDirection <$> filterJust (arrowKeyToDirection <$> UI.keydown body)
        -- Look ahead one step since we want to level up *when* capturing, not *after* capturing
        eLevelUp = levelup <$ filterE (doesCapture . step) (bApp <@ eClock)
        eApp = concatenate <$> unions [eStep, eReset, eChangeDirection, eLevelUp]
    bApp <- accumB initialState eApp

    onChanges bApp $ render page
    element (scoreDisplay page) # sink UI.text (("Score: " ++) . show . score <$> bApp)
    pure ()

arrowKeyToDirection :: UI.KeyCode -> Maybe Direction
arrowKeyToDirection keyCode = case keyCode of
    37 -> Just L
    38 -> Just U
    39 -> Just R
    40 -> Just D
    _  -> Nothing
