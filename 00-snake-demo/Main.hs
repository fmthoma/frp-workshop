{-# LANGUAGE RecursiveDo #-}
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Data.Foldable (for_)
import Data.Traversable (for)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import System.Random (mkStdGen, randomRs)

main :: IO ()
main = UI.startGUI UI.defaultConfig { jsStatic = Just "." } setup

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
        #. "bg-light rounded"
        # set UI.width 400
        # set UI.height 400
    resetButton <- UI.button
        # set UI.text "Reset"
        #. "btn btn-block btn-secondary"
    scoreDisplay <- UI.new
        #. "btn font-weight-bold text-white"
    gridElement <- flexboxGrid
        [ [ col 12 canvas ]
        , [ col 6 scoreDisplay, col 6 resetButton ] ]
        # set style [("width", "430px"), ("margin-top", "100px")]
    getBody window
        #. "text-center bg-dark"
        #+ [ element gridElement ]
    UI.addStyleSheet window "bootstrap.css"
    pure Page { canvas = canvas, resetButton = resetButton, scoreDisplay = scoreDisplay }

render :: Page -> ApplicationState -> UI ()
render page state@(ApplicationState { snake = Snake points, item = item }) = do
    let snakeColor = if isAlive state then "black" else "red"
    element (resetButton page)
        #. ("btn btn-block btn-" ++ if isAlive state then "secondary" else "primary")
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

flexboxGrid :: [[(Int, Element)]] -> UI Element
flexboxGrid rows = do
    container <- UI.new #. "container"
    rowElements <- for rows $ \cols -> do
        colElements <- for cols $ \(width, content) ->
            UI.new #. ("col-" ++ show width) # set UI.children [content]
        UI.new #. "row" # set UI.children colElements
    element container # set UI.children rowElements

col :: Int -> Element -> (Int, Element)
col = curry id

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
