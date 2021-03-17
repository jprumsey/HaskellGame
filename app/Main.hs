module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game


-- TODO: fix game so that movement happens when a key is /held/, not just when it is pressed individually (kind of tricky with event keys) https://stackoverflow.com/questions/52871673/haskell-gloss-do-something-every-frame-key-is-pressed
------------------------------ STATE -------------------------------

-- All ships - the player and enemies - are triangles
-- All projectiles are circles whose health should be initialized to 1 and decremented to 0 upon use
data Entity = Entity { position :: (Float, Float),
                     velocity :: (Float, Float),
                     health :: Int 
                     }


-- TODO: add to state: active weapon
data ShooterGame = Game
    { 
        player :: Entity,
        enemies :: [Entity],
        playerProjectiles :: [Entity],
        enemyProjectiles :: [Entity],
        paused :: Bool
    }

initialState :: ShooterGame
initialState = Game
    {
        player = Entity (0, -fromIntegral width/2 + fromIntegral offset) (0,0) 5,
        enemies = [],
        playerProjectiles = [],
        enemyProjectiles = [],
        paused = False
    }

width, height, offset :: Int
width = 500
height = 500
offset = 50

-- Steps player takes per frame, triangular side length of player
playerStep, playerSideLength :: Float
playerStep = 5
playerSideLength = 60

background :: Color
background = black

window :: Display
window = InWindow "TitleHere" (width, height) (offset, offset)

fps :: Int
fps = 60

update :: Float -> ShooterGame -> ShooterGame
update seconds game = game

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
handleKeys (EventKey (Char 'w') _ _ _) game =
    if y < fromIntegral width / 2
        then game { player = Entity (x, y + playerStep) (0,0) (health $ player game)} else game
    where
        (x, y) = position $ player game
handleKeys (EventKey (Char 's') _ _ _) game =
    if y > -fromIntegral width / 2
        then game { player = Entity (x, y - playerStep) (0,0) (health $ player game)} else game
    where
        (x, y) = position $ player game
handleKeys (EventKey (Char 'a') _ _ _) game =
    if x > -fromIntegral width / 2
        then game { player = Entity (x - playerStep, y) (0,0) (health $ player game)} else game
    where
        (x, y) = position $ player game
handleKeys (EventKey (Char 'd') _ _ _) game =
    if x < fromIntegral width / 2
        then game { player = Entity (x + playerStep, y) (0,0) (health $ player game)} else game
    where
        (x, y) = position $ player game

-- TODO: weapon change

-- For a 'p' keypress, pause or unpause the game
handleKeys (EventKey (Char 'p') Down _ _) game =
 game { paused = (not (paused game)) }

handleKeys _ game = game

main :: IO ()
main = play window background fps initialState render handleKeys update
