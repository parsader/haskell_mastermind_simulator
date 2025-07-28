import Test.Tasty
import Test.Tasty.HUnit
import Game
import Graphics.Gloss

main :: IO ()
main = defaultMain $ testGroup "Tests" $
  [ testCase "Addition works" $ do
      2 + 3 @?= 5  -- actual @?= expected
  , testCase "Multiplication works" $ do
      6 @=? 2 * 3  -- expected @=? actual
  , testCase "Initial State Returns Code" $ do
      initialState @?= newDefaultCode
  , testCase "Update returns same state" $ do
      update 0.7 newDefaultCode @?= newDefaultCode
  , testCase "integer to peg colour red" $ do
      integerToPegColor 0 @?= Red
    , testCase "integer to peg colour orange" $ do
      integerToPegColor 1 @?= Orange
      , testCase "integer to peg colour yellow" $ do
      integerToPegColor 2 @?= Yellow
      , testCase "integer to peg colour Green" $ do
      integerToPegColor 3 @?= Green
      , testCase "integer to peg colour Blue" $ do
      integerToPegColor 4 @?= Blue
      , testCase "increments Peg Colour Red" $ do
      nextPegColor Red @?= Orange
      , testCase "increments Peg Colour Orange" $ do
      nextPegColor Orange @?= Yellow
      , testCase "increments Peg Colour Yellow" $ do
      nextPegColor Yellow @?= Green
      , testCase "increments Peg Colour Green" $ do
      nextPegColor Green @?= Blue
      , testCase "increments Peg Colour Blue" $ do
      nextPegColor Blue @?= Red
      , testCase "increments Peg Colour Blue" $ do
      nextPegColor Blue @?= Red
      , testCase "Checks that game is lost" $ do
      isGameWon (GameState newDefaultCode [Code Red Blue Red Red] 0) @?= -1
      , testCase "Checks that game is ongoing (no Guesses)" $ do
      isGameWon (newDefaultGameState newDefaultCode) @?= 0
      , testCase "Checks that game is ongoing (more guesses available)" $ do
      isGameWon (GameState newDefaultCode [Code Red Blue Red Red] 11) @?= 0
      , testCase "Checks that game is Won" $ do
      isGameWon (GameState newDefaultCode [newDefaultCode] 11) @?= 1
      , testCase "Added guess is added and decreases remaining guesses" $ do
      addGuess (newDefaultGameState newDefaultCode) newDefaultCode @?= GameState newDefaultCode [newDefaultCode] 11
      , testCase "correctly generates new default game" $ do
      newDefaultGameState newDefaultCode @?= GameState newDefaultCode [] 12
      , testCase "correctly generates new default code" $ do
      newDefaultCode @?= Code Red Red Red Red
      , testCase "correctly chages pin (no Change)" $ do
      changePinInPosition newDefaultCode (-1) @?= newDefaultCode
      , testCase "correctly chages pin (first pin)" $ do
      changePinInPosition newDefaultCode 0 @?= Code Orange Red Red Red
      , testCase "correctly chages pin (second pin)" $ do
      changePinInPosition newDefaultCode 1 @?= Code Red Orange Red Red
      , testCase "correctly chages pin (third pin)" $ do
      changePinInPosition newDefaultCode 2 @?= Code Red Red Orange Red
      , testCase "correctly chages pin (fourth pin)" $ do
      changePinInPosition newDefaultCode 3 @?= Code Red Red Red Orange
      , testCase "correctly chages pin (Empty pin is Empty)" $ do
      changePinInPosition Empty 3 @?= Empty
      , testCase "Draws Peg (Red)" $ do
      drawPeg Red @?= (color red $ circleSolid pinRad)
      , testCase "Draws Peg (Orange)" $ do
      drawPeg Orange @?= (color orange $ circleSolid pinRad)
      , testCase "Draws Peg (Yellow)" $ do
      drawPeg Yellow @?= (color yellow $ circleSolid pinRad)
      , testCase "Draws Peg (Green)" $ do
      drawPeg Green @?= (color green $ circleSolid pinRad)
      , testCase "Draws Peg (Blue)" $ do
      drawPeg Blue @?= (color blue $ circleSolid pinRad)
      , testCase "Draws Code (Not Empty)" $ do
      drawCode newDefaultCode @?= pictures [
        translate (((5*pinSpace + 8 * pinRad)/2) - (pinRad + pinSpace)) 0 $ rectangleWire (5*pinSpace + 8 * pinRad) (2 * pinSpace + 2* pinRad),
        translate 0 0 $ drawPeg Red,
        translate (2*pinRad+pinSpace) 0 $ drawPeg Red,
        translate (2*(2*pinRad+pinSpace)) 0 $ drawPeg Red,
        translate (3*(2*pinRad+pinSpace)) 0 $ drawPeg Red]
      , testCase "Draws Code (Empty)" $ do
      drawCode Empty @?= pictures [
        translate (((5*pinSpace + 8 * pinRad)/2) - (pinRad + pinSpace)) 0 $ rectangleWire (5*pinSpace + 8 * pinRad) (2 * pinSpace + 2* pinRad),
        translate 0 0 $ Circle pinRad,
        translate (pinRad+pinSpace) 0 $ Circle pinRad,
        translate (2*(2*pinRad+pinSpace)) 0 $ Circle pinRad,
        translate (3*(2*pinRad+pinSpace)) 0 $ Circle pinRad]
      , testCase "Get Dist no distance" $ do
        getDist (1,2) (1,2) @?= 0
      , testCase "Get Dist positive distance" $ do
        getDist (0,0) (2,2) @?= sqrt 8
      , testCase "Get Dist negative distance" $ do
        getDist (0,0) (-2,-2) @?= sqrt 8
      , testCase "Get Dist arbitrary" $ do
        getDist (-17,32) (-2,-23) @?= sqrt 3250
      , testCase "calculate pin (button clicked)" $ do
        calculatePin 100 (0, 0) [(10, 10)] @?= [True]
      , testCase "calculate pin (button not clicked)" $ do
        calculatePin 1 (0, 0) [(10, 10)] @?= [False]
      , testCase "calculate pin (button clicked multiple buttons)" $ do
        calculatePin 100 (0, 0) [(-100, -100), (10, 10), (100, 100)] @?= [False, True, False]
      , testCase "getTrueLoc empty list" $ do
      getTrueLoc [] @?= -1
      , testCase "getTrueLoc all False" $ do
      getTrueLoc [False, False, False] @?= -1
      , testCase "getTrueLoc True in one item list" $ do
      getTrueLoc [True] @?= 0
      , testCase "getTrueLoc True first val in multiple item list" $ do
      getTrueLoc [True, False, False] @?= 0
      , testCase "getTrueLoc True last val in multiple item list" $ do
      getTrueLoc [False,False, True ] @?= 2
      , testCase "getTrueLoc True middle val in multiple item list" $ do
      getTrueLoc [False, True, False] @?= 1
  -- NOTE: Uncomment this to see what a failing assertion looks like:
  -- , testCase "Bad assertion" $ do
  --     1 @?= 2
  -- NOTE: This is how to explicitly assert failures:
  -- , testCase "Explicit failure" $ do
  --     assertFailure "BOOM!"
  ]