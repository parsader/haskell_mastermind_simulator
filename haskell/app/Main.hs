module Main where

import Graphics.Gloss
import Game
import System.Random hiding (Random)
--import Lib
windowDisplay :: Display
windowDisplay = InWindow "Window" (500, 500) (10, 10)

background :: Color
background = white

randomInt :: Integer -> Integer -> IO Integer
randomInt x y = getStdRandom (randomR (x,y))

getRandomCode :: Integer -> Integer -> Integer -> Integer -> Code
getRandomCode int1 int2 int3 int4 = Code (integerToPegColor int1) (integerToPegColor int2) (integerToPegColor int3) (integerToPegColor int4) 

main :: IO ()
main = do
    randInt1 <- randomInt 0 4
    randInt2 <- randomInt 0 4
    randInt3 <- randomInt 0 4
    randInt4 <- randomInt 0 4
    let randomCode = getRandomCode randInt1 randInt2 randInt3 randInt4
    play windowDisplay background 120 (newDefaultGameState randomCode) drawGame eventHandler update


-- play windowDisplay background 120 (newDefaultGameState newDefaultCode) drawGame eventHandler update