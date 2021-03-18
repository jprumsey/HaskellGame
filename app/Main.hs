module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game


------------------------------ STATE -------------------------------

-- The player is a triangle
-- The ships are squares
-- All projectiles are circles whose health should be initialized to 1 and decremented to 0 upon use
data Entity = Entity { position :: (Float, Float),
                     velocity :: (Float, Float),
                     health :: Int 
                     } deriving Show


-- TODO: add to state: active weapon
data ShooterGame = Game
    { 
        player :: Entity,
        enemies :: [Entity],
        playerProjectiles :: [Entity],
        enemyProjectiles :: [Entity],
        paused :: Bool,
        keysDown :: (Bool, Bool, Bool, Bool)
    } deriving Show

initialState :: ShooterGame
initialState = Game
    {
        player = Entity (0, -fromIntegral width/2 + fromIntegral offset) (150,150) 5,
        enemies = [],
        playerProjectiles = [],
        enemyProjectiles = [],
        paused = False,
        keysDown = (False, False, False, False)
    }

width, height, offset :: Int
width = 500
height = 500
offset = 50

-- Steps player takes per frame, triangular side length of player
playerSideLength, enemyBaseLength :: Float
playerSideLength = 30
enemyBaseLength = 15

background :: Color
background = black

window :: Display
window = InWindow "TitleHere" (width, height) (offset, offset)

fps :: Int
fps = 60

update :: Float -> ShooterGame -> ShooterGame
update seconds game = movePlayer seconds game

------------------------------ RENDERING -------------------------------
-- Path representing an equilateral triangle centered about origin, with vertex pointed upwards
trianglePath :: Float -> Path
trianglePath sideLength
    = let topCorner   = 0.43301270189 * sideLength
          rightCorner = sideLength / 2 
    in [(-rightCorner, -topCorner), (rightCorner, -topCorner), (0, topCorner)]

-- Solid triangle centered about origin
triangleSolid :: Float -> Picture
triangleSolid sideLength
    = Polygon $ trianglePath sideLength

render :: ShooterGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [playerShip]
  where
    playerShip = uncurry translate (position $ player game) $ color playerColor $ triangleSolid playerSideLength
    playerColor = dark red


------------------------------ CONTROLS -------------------------------
handleKeys :: Event -> ShooterGame -> ShooterGame
handleKeys (EventKey (Char 'w') state _ _) game = game { keysDown = updatedKeys }
    where
        (_, a, s, d) = keysDown game
        updatedKeys = if state == Down then (True, a, s, d) else (False, a, s, d)
handleKeys (EventKey (Char 's') state _ _) game = game { keysDown = updatedKeys }
    where
        (w, a, _, d) = keysDown game
        updatedKeys = if state == Down then (w, a, True, d) else (w, a, False, d)
handleKeys (EventKey (Char 'a') state _ _) game = game { keysDown = updatedKeys }
    where
        (w, _, s, d) = keysDown game
        updatedKeys = if state == Down then (w, True, s, d) else (w, False, s, d)
handleKeys (EventKey (Char 'd') state _ _) game = game { keysDown = updatedKeys }
    where
        (w, a, s, _) = keysDown game
        updatedKeys = if state == Down then (w, a, s, True) else (w, a, s, False)

-- TODO: weapon change

-- For a 'p' keypress, pause or unpause the game
handleKeys (EventKey (Char 'p') Down _ _) game =
 game { paused = (not (paused game)) }

handleKeys _ game = game

movePlayer :: Float -> ShooterGame -> ShooterGame
movePlayer seconds game = game { player = newPlayer }
    where
        (w, a, s, d) = keysDown game
        playerEnt = player game
        (xPos, yPos) = position playerEnt
        (xVel, yVel) = velocity playerEnt
        xPos' = xPos + fromIntegral ( fromEnum d - fromEnum a ) * xVel * seconds
        yPos' = yPos + fromIntegral ( fromEnum w - fromEnum s ) * yVel * seconds
        newPlayer = playerEnt { position = putInBounds (xPos', yPos') ( getCircRadius playerSideLength 3 ) }

getCircRadius :: Float -> Float -> Float
getCircRadius sideLength numSides = sideLength / ( 2 * cos ( pi / numSides ) )

putInBounds :: (Float, Float) -> Float -> (Float, Float)
putInBounds (xPos, yPos) radius = (xPos', yPos')
    where
        xPos' = min ( max ( xPos ) ( -fromIntegral width  / 2 + radius / 2 ) ) ( fromIntegral width  / 2 - radius / 2 )
        yPos' = min ( max ( yPos ) ( -fromIntegral height / 2 + radius / 2 ) ) ( fromIntegral height / 2 - radius / 2 )

main :: IO ()
main = play window background fps initialState render handleKeys update
