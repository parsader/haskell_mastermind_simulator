module Constants (
    pinRad,
    pinSpace,
    guessScale,
    gameTransX,
    gameTransY
 ) where


-- Radius of a Pin
pinRad :: Float 
pinRad = 20

-- Spacing Between Pins
pinSpace :: Float
pinSpace = 5

-- The scale of the previous guesses
guessScale :: Float
guessScale = 1

-- Translates whole Game in the X
gameTransX :: Float
gameTransX = -160

-- Translates whole Game in the Y
gameTransY :: Float
gameTransY = -220