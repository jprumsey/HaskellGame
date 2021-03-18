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
                     health :: Int,
                     numSides :: Float,
                     sideLength :: Float
                     } deriving Show


-- TODO: add to state: score
data ShooterGame = Game
    { 
        player :: Entity,
        enemies :: [Entity],
        playerProjectiles :: [Entity],
        enemyProjectiles :: [Entity],
        paused :: Bool,
        keysDown :: (Bool, Bool, Bool, Bool, Bool),
        activeWeapon :: Int
    } deriving Show

initialState :: ShooterGame
initialState = Game
    {
        player = Entity (0, -fromIntegral width/2 + fromIntegral offset) (150,150) 5 3 playerSideLength,
        enemies = [ Entity (0, 0) (30, 0) 1 4 enemyBaseLength ],
        playerProjectiles = [],
        enemyProjectiles = [],
        paused = False,
        keysDown = (False, False, False, False, False),
        activeWeapon = weapon1
    }

width, height, offset :: Int
width = 500
height = 500
offset = 50

weapon1, weapon2, weapon3 :: Int
weapon1 = 0
weapon2 = 1
weapon3 = 2

-- Steps player takes per frame, triangular side length of player
playerSideLength, enemyBaseLength, projectileRadius :: Float
playerSideLength = 30
enemyBaseLength = 15
projectileRadius = 2

background :: Color
background = black

window :: Display
window = InWindow "TitleHere" (width, height) (offset, offset)

fps :: Int
fps = 60

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
  pictures [playerShip,
            enemyShips,
            playerProj
            ]
  where
    -- the player
    playerShip = uncurry translate (position $ player game) $ color playerColor $ triangleSolid playerSideLength
    playerColor = dark red

    -- the enemies
    enemyShips = pictures ( map renderEnemy ( enemies game ) )
    renderEnemy :: Entity -> Picture
    renderEnemy ent = 
        uncurry translate (position ent) $ color enemyColor $ rectangleSolid enemyBaseLength enemyBaseLength

    enemyColor = dark blue -- TODO: fix so it can use multiple enemies

    playerProj = pictures ( map (renderProjectile projectileRadius playerProjectileColor) (playerProjectiles game))
    renderProjectile :: Float -> Color -> Entity -> Picture
    renderProjectile radius col ent =
        uncurry translate (position ent) $ color col $ circleSolid radius

    playerProjectileColor = white
    


------------------------------ CONTROLS -------------------------------
handleKeys :: Event -> ShooterGame -> ShooterGame
handleKeys (EventKey (Char 'w') state _ _) game = game { keysDown = updatedKeys }
    where
        (_, a, s, d, sp) = keysDown game
        updatedKeys = if state == Down then (True, a, s, d, sp) else (False, a, s, d, sp)
handleKeys (EventKey (Char 's') state _ _) game = game { keysDown = updatedKeys }
    where
        (w, a, _, d, sp) = keysDown game
        updatedKeys = if state == Down then (w, a, True, d, sp) else (w, a, False, d, sp)
handleKeys (EventKey (Char 'a') state _ _) game = game { keysDown = updatedKeys }
    where
        (w, _, s, d, sp) = keysDown game
        updatedKeys = if state == Down then (w, True, s, d, sp) else (w, False, s, d, sp)
handleKeys (EventKey (Char 'd') state _ _) game = game { keysDown = updatedKeys }
    where
        (w, a, s, _, sp) = keysDown game
        updatedKeys = if state == Down then (w, a, s, True, sp) else (w, a, s, False, sp)
handleKeys (EventKey (Char 't') state _ _) game = game { keysDown = updatedKeys }
    where
        (w, a, s, d, _) = keysDown game
        updatedKeys = if state == Down then (w, a, s, d, True) else (w, a, s, d, False)


-- TODO: after basics: weapon change

-- For a 'p' keypress, pause or unpause the game
handleKeys (EventKey (Char 'p') Down _ _) game =
 game { paused = (not (paused game)) }

handleKeys _ game = game

------------------------------ UPDATES -------------------------------

update :: Float -> ShooterGame -> ShooterGame
update seconds game = (runUpdates . moveEntities seconds) game

-- moves everything
moveEntities :: Float -> ShooterGame -> ShooterGame
moveEntities seconds game = 
    game { 
        enemies = map (moveNonPlayer seconds) (enemies game),
        player = movePlayer seconds (keysDown game) (player game),
        playerProjectiles = map (moveNonPlayer seconds) (playerProjectiles game)
        }

-- moves player
movePlayer :: Float -> (Bool, Bool, Bool, Bool, Bool) -> Entity -> Entity
movePlayer seconds (w, a, s, d, sp) ent = ent { position = newPos }
    where
        (xPos, yPos) = position ent
        (xVel, yVel) = velocity ent
        xPos' = xPos + fromIntegral ( fromEnum d - fromEnum a ) * xVel * seconds
        yPos' = yPos + fromIntegral ( fromEnum w - fromEnum s ) * yVel * seconds
        newPos = putInBounds (xPos', yPos') ( getCircRadius ent )

-- moves all other entities
moveNonPlayer :: Float -> Entity -> Entity
moveNonPlayer seconds ent = ent { position = (xPos', yPos') }
    where
        (xPos, yPos) = position ent
        (xVel, yVel) = velocity ent
        xPos' = xPos + xVel * seconds
        yPos' = yPos + yVel * seconds

-- add additional weapons here
firePlayerProjectiles :: Bool -> Int -> (Float, Float) -> [Entity] -> [Entity]
firePlayerProjectiles fireKeyDown weaponType position projectiles = 
    if fireKeyDown
        then 
            if weaponType == weapon1
                then
                    -- delay here?
                    (Entity position (0,150) 1 3 projectileRadius):projectiles
            else projectiles
    else
        projectiles

-- TODO: put logic for adding new enemies and enemy projectiles here
runUpdates :: ShooterGame -> ShooterGame
runUpdates game =
    -- TODO: remove "dead" entities (health <= 0)
    game { playerProjectiles = firePlayerProjectiles space (activeWeapon game) (x, y) (playerProjectiles game)}
    where
        (x, y) = position $ player game
        (w, a, s, d, space) = keysDown game

-- Will be useful when determining if projectiles should be removed
outOfBounds :: Entity -> Bool
outOfBounds ent = xOOB || yOOB
    where
        (xPos, yPos) = position ent
        r = getCircRadius ent
        xOOB = ( ( xPos - r ) < ( -fromIntegral width  / 2 ) ) || ( ( xPos + r ) > ( fromIntegral width  / 2 ) )
        yOOB = ( ( yPos - r ) < ( -fromIntegral height / 2 ) ) || ( ( yPos + r ) > ( fromIntegral height / 2 ) )

-- Gets the circumradius of the described regular polynomial, use this for collisions
getCircRadius :: Entity -> Float
getCircRadius ent = ( sideLength ent ) / ( 2 * cos ( pi / ( numSides ent ) ) )

-- prevents xPos and yPos from going beyond the screen, use this for the player movement
putInBounds :: (Float, Float) -> Float -> (Float, Float)
putInBounds (xPos, yPos) radius = (xPos', yPos')
    where
        xPos' = min ( max ( xPos ) ( -fromIntegral width  / 2 + radius / 2 ) ) ( fromIntegral width  / 2 - radius / 2 )
        yPos' = min ( max ( yPos ) ( -fromIntegral height / 2 + radius / 2 ) ) ( fromIntegral height / 2 - radius / 2 )

main :: IO ()
main = play window background fps initialState render handleKeys update
