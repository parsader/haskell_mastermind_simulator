module Game
    (
        GameState(GameState),
        Peg(Red,Orange,Yellow,Green,Blue),
        Code(Code, Empty),
        integerToPegColor,
        nextPegColor,
        isGameWon,
        addGuess,
        newDefaultGameState,
        newDefaultCode,
        changePinInPosition,
        drawPeg,
        drawGame,
        drawCode,
        getDist,
        initialState,
        update,
        getTrueLoc,
        pinSpace,
        pinRad,
        pinLoc,
        calculatePin,
        eventHandler
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
    ( Event(EventKey),
      Key(MouseButton),
      MouseButton(LeftButton),
      KeyState(Down) )
import Graphics.Gloss.Data.ViewPort

import Constants

import System.Random hiding (Random)

import Data.Time.Clock

initialState :: GameState
initialState = newDefaultGameState newDefaultCode

update :: Float-> GameState -> GameState
update _ c = c

-- a game state that represents the correct code,
-- the current guess being worked on
-- a list of guesses made so far with most recent guess in the front
-- and a integer with remaining guesses 
data GameState = GameState Code Code [Code] Integer deriving (Eq, Show)

-- A peg representing a color
data Peg = Red | Orange | Yellow | Green | Blue deriving (Eq, Show)

-- A code of four pegs representing a guess or correct Code
data Code = Code Peg Peg Peg Peg | Empty deriving (Eq, Show)

data ScoreCounter = Correct | WrongPlace | Wrong | NA deriving (Eq, Show)

data Score = Score ScoreCounter ScoreCounter ScoreCounter ScoreCounter deriving (Eq, Show)

data ScoringHelper = ScoringHelper [Peg] [Peg] [ScoreCounter]

-- takes an integer and returns a corresponding peg color
integerToPegColor :: Integer -> Peg
integerToPegColor i = iToC (rem i 5)
    where
        iToC 0 = Red
        iToC 1 = Orange
        iToC 2 = Yellow
        iToC 3 = Green
        iToC _ = Blue

-- takes a peg and gives the next peg colour in the cycle
nextPegColor :: Peg -> Peg
nextPegColor Red = Orange
nextPegColor Orange = Yellow
nextPegColor Yellow = Green
nextPegColor Green = Blue
nextPegColor _ = Red

-- takes a game and returns 1 if guesser has won, 
-- -1 if mastermind won, 
-- and 0 otherwise
isGameWon :: GameState -> Integer
isGameWon (GameState _ _ [] _) = 0
isGameWon (GameState correct _ (x:xs) rg)
    |x == correct = 1
    |rg > 0 = 0
    |otherwise = -1

-- takes a current game state and a guessed Code
-- and returns a new game state including that guess
addGuess :: GameState -> GameState
addGuess (GameState ap cg ps i) = GameState ap newDefaultCode (cg:ps) (i - 1)

-- takes a Code and makes a new default game state
-- with patern as actual code
newDefaultGameState :: Code -> GameState
newDefaultGameState p = GameState p newDefaultCode [] 12

-- makes a new default Code of all red
newDefaultCode :: Code
newDefaultCode = Code Red Red Red Red
--newDefaultCode = getRandomCode 1

--newDefaultCode = (Code integerToPegColor(1)  integerToPegColor(1)  integerToPegColor(1)  integerToPegColor(1))

-- type Random a = Rand StdGen a

-- rollDie :: Integer ->  Integer
-- rollDie n = randomR (1, n)

drawInt :: Integer -> Integer -> IO Integer
drawInt x y = getStdRandom (randomR (x,y))

-- getRandomInt :: IO Integer -> Integer
-- getRandomInt IO int = int

-- >>= :: IO Integer-> (Integer -> IO Integer) -> IO Integer

rand :: Integer -> Integer
rand seed = (1103515245 * seed + 102941 * seed - 2134) `mod` 5

-- currTime <- getCurrentTime
-- let timed = floor $ utctDayTime currTime :: Int

integralTime :: (Integral a) => IO a
integralTime = getCurrentTime >>= return.floor.utctDayTime

time = getCurrentTime >>= return . utctDayTime

diffTimeToSeconds :: DiffTime -> Integer
diffTimeToSeconds = floor . toRational

----getRandomCode :: Integer -> Code
---getRandomCode int = Code (integerToPegColor (rand 5478))  (integerToPegColor (rand 5678))  (integerToPegColor (rand 910112))   (integerToPegColor (rand 131415)) 


-- increments the pin colour of the pin in the given position
changePinInPosition :: Code -> Integer -> Code
changePinInPosition c (-1) = c
changePinInPosition (Code f s t fth) 0 = Code (nextPegColor f) s t fth
changePinInPosition (Code f s t fth) 1 = Code f (nextPegColor s) t fth
changePinInPosition (Code f s t fth) 2 = Code f s (nextPegColor t) fth
changePinInPosition (Code f s t fth) _ = Code f s t (nextPegColor fth)
changePinInPosition Empty _ = Empty

-- Takes a peg and produces a picture of it
drawPeg :: Peg -> Picture
drawPeg Red = color red $ circleSolid pinRad
drawPeg Orange = color orange $ circleSolid pinRad
drawPeg Yellow = color yellow $ circleSolid pinRad
drawPeg Green = color green $ circleSolid pinRad
drawPeg Blue = color blue $ circleSolid pinRad

drawGame :: GameState -> Picture
drawGame (GameState key guess pastGuess int) = pictures [(pictures [drawButton, drawCode guess]),
 (pictures (drawCodes pastGuess 1)),
 drawIsCorrect key pastGuess,
 drawGuessesRemaining int]

drawGuessesRemaining :: Integer -> Picture
drawGuessesRemaining int = translate ((5*pinSpace + 8 * pinRad)/2 + 75 + gameTransX) gameTransY $ scale 0.1 0.1 $ text ("Guesses remaining: " ++ (show int))





-- isGameWon :: GameState -> Integer
-- isGameWon (GameState _ _ [] _) = 0
-- isGameWon (GameState correct _ (x:xs) rg)
--    |x == correct = 1
--   |rg > 0 = 0
--    |otherwise = -1

-- drawIsCorrect :: Integer -> Picture
-- drawIsCorrect 1 = translate ((5*pinSpace + 8 * pinRad)/2 + 75 + gameTransX) (50+ gameTransY) $ scale 0.1 0.1 $ text ("Guess is Correct")
-- drawIsCorrect 0 = translate ((5*pinSpace + 8 * pinRad)/2 + 75 + gameTransX) (50+ gameTransY) $ scale 0.1 0.1 $ text ("Keep Guessing")
-- drawIsCorrect -1 = translate ((5*pinSpace + 8 * pinRad)/2 + 75 + gameTransX) (50+ gameTransY) $ scale 0.1 0.1 $ text ("Ran out of guesses")

drawIsCorrect :: Code -> [Code] -> Picture
drawIsCorrect (Code g1 g2 g3 g4) [] =  translate ((5*pinSpace + 8 * pinRad)/2 + 75 + gameTransX) (50+ gameTransY) $ scale 0.1 0.1 $ text ("Make your first guess")
drawIsCorrect (Code g1 g2 g3 g4) ((Code c1 c2 c3 c4):codes)
    | g1 == c1 && g2 == c2 && g3 == c3 && g4 == c4 = translate ((5*pinSpace + 8 * pinRad)/2 + 75 + gameTransX) (50+ gameTransY) $ scale 0.1 0.1 $ text ("Guess is Correct :)")
    | otherwise = translate ((5*pinSpace + 8 * pinRad)/2 + 75 + gameTransX) (50+ gameTransY) $ scale 0.1 0.1 $ text ("Guess is wrong :(")







drawButton :: Picture
drawButton = translate (-(2*pinRad + (pinRad + pinSpace)) + gameTransX) gameTransY $ pictures [
    color (dark green) $ rectangleSolid (2* pinRad) (2* pinRad),
    translate (-15) (-1) $ scale 0.08 0.08 $ text "Guess!"]

drawCodes :: [Code] -> Float -> [Picture]
drawCodes [] _ = []
drawCodes (c:cs) i =  (translate 0 (2*i*(pinRad + (2 * pinSpace))) $ (scale guessScale guessScale $ drawCode c)) : drawCodes cs (i+1)

-- fixEval :: [Peg] -> [Peg] -> [ScoreCounter] -> Score
-- fixEval (g:gs) (c:cs) = 

checkWrongPlace :: Peg -> [Peg] -> ScoreCounter
checkWrongPlace _ [] = Wrong
checkWrongPlace p (c:cs)
    |p == c = WrongPlace
    |otherwise = checkWrongPlace p cs


evalCode :: Code -> Code -> Score 
evalCode Empty _ = (Score Wrong Wrong Wrong Wrong)
evalCode _ Empty = (Score Wrong Wrong Wrong Wrong)
evalCode (Code g1 g2 g3 g4) (Code c1 c2 c3 c4) = (Score (evalPeg g1 c1) (evalPeg g2 c2) (evalPeg g3 c3) (evalPeg g4 c4))

evalPeg :: Peg -> Peg -> ScoreCounter
evalPeg p1 p2
    | p1 == p2 = Correct
    | otherwise = NA




--takes a code and produces a picture of it
drawCode :: Code -> Picture
drawCode (Code p1 p2 p3 p4) = pictures [
    translate (((5*pinSpace + 8 * pinRad)/2) - (pinRad + pinSpace) + gameTransX) (0 + gameTransY) $ rectangleWire (5*pinSpace + 8 * pinRad) (2 * pinSpace + 2* pinRad),
    translate (0 + gameTransX) (0 + gameTransY) $ drawPeg p1,
    translate (2*pinRad+pinSpace + gameTransX) (0 + gameTransY) $ drawPeg p2,
    translate (2*(2*pinRad+pinSpace) + gameTransX) (0 + gameTransY) $ drawPeg p3,
    translate (3*(2*pinRad+pinSpace) + gameTransX) (0 + gameTransY) $ drawPeg p4]
drawCode Empty = pictures [
    translate (((5*pinSpace + 8 * pinRad)/2) - (pinRad + pinSpace) + gameTransX) 0 $ rectangleWire (5*pinSpace + 8 * pinRad) (2 * pinSpace + 2* pinRad),
    translate (0 + gameTransX) (0 + gameTransY) $ Circle pinRad,
    translate (pinRad+pinSpace + gameTransX) (0 + gameTransY) $ Circle pinRad,
    translate (2*(2*pinRad+pinSpace) + gameTransX) (0 + gameTransY) $ Circle pinRad,
    translate (3*(2*pinRad+pinSpace) + gameTransX) (0 + gameTransY) $ Circle pinRad]

-- Handles mouse events and causes neccisarry changes to game state
eventHandler :: Event -> GameState -> GameState
eventHandler (EventKey (MouseButton LeftButton) Down _ loc) (GameState key guess pastGuess num) = buttonClick loc (GameState key (changePinInPosition guess (getTrueLoc (calculatePin pinRad loc pinLoc))) pastGuess num)
eventHandler _ gs = gs

buttonClick :: (Float, Float) -> GameState -> GameState
buttonClick _ (GameState key guess pastGuess 0) = (GameState key guess pastGuess 0)
buttonClick (x, y) (GameState key guess pastGuess num)
    | x <= (-(2*pinRad + pinSpace)) + gameTransX  && x >= (-((4*pinRad) + pinSpace)) + gameTransX &&
     y <= pinRad + gameTransY && y >= (-pinRad) + gameTransY = (GameState key newDefaultCode (guess:pastGuess) (num-1))
buttonClick _ gs = gs

-- calculates euclidean distance between two points
getDist :: (Float, Float) -> (Float, Float) -> Float
getDist (x1, y1) (x2, y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

-- Returns a list of bools where clicked objects are true
calculatePin :: Float -> (Float, Float) -> [(Float, Float)] -> [Bool]
calculatePin thresh mouse = map checkRange
    where
        checkRange (x, y) = getDist mouse (x + gameTransX, y + gameTransY) <= thresh

-- An array of pin locations in Proof of Concept
pinLoc :: [(Float, Float)]
pinLoc = [(0,0), (2*pinRad+pinSpace,0), (2*(2*pinRad+pinSpace),0), (3*(2*pinRad+pinSpace),0)]

--Converts a list of boolean "clicked" values to index number
getTrueLoc :: [Bool] -> Integer
getTrueLoc = helper 0
    where
        helper _ [] = -1
        helper i (b:bs)
            |b = i
            |otherwise = helper (i+1) bs