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
                     evelocity :: (Float, Float),
                     health :: Int,
                     numSides :: Float,
                     sideLength :: Float
                     } deriving Show

data Weapon = Weapon { wvelocity :: (Float, Float),
                       damage :: Int
                     } deriving Show

-- TODO: add to state: score
data ShooterGame = Game
    { 
        frameCount :: Int,
        player :: Entity,
        enemies :: [Entity],
        playerProjectiles :: [Entity],
        enemyProjectiles :: [Entity],
        paused :: Bool,
        keysDown :: (Bool, Bool, Bool, Bool, Bool),
        activeWeapon :: Weapon
    } deriving Show

initialState :: ShooterGame
initialState = Game
    {
        frameCount = 0,
        player = Entity (0, -fromIntegral width/2 + fromIntegral offset) (100,100) 5 3 playerSideLength,
        enemies = [],
        playerProjectiles = [],
        enemyProjectiles = [],
        paused = False,
        keysDown = (False, False, False, False, False),
        activeWeapon = pWeapon1
    }

width, height, offset :: Int
width = 500
height = 500
offset = 50

eWeapon1, pWeapon1 :: Weapon
eWeapon1 = Weapon { wvelocity = (0, -150), damage = 1 }
pWeapon1 = Weapon { wvelocity = (0,  150), damage = 1 }

playerFireRate, enemyFireRate, enemySpawnRate :: Int
playerFireRate = 10
enemyFireRate = 50
enemySpawnRate = 600

-- Steps player takes per frame, triangular side length of player
playerSideLength, enemyBaseLength, spawnOffset, projectileRadius :: Float
playerSideLength = 30
enemyBaseLength = 20
spawnOffset = 10
projectileRadius = 2

background :: Color
background = black

window :: Display
window = InWindow "Square Shooter 5000" (width, height) (offset, offset)

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
            playerProj,
            enemyProj
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
    enemyProj  = pictures ( map (renderProjectile projectileRadius enemyProjectileColor ) (enemyProjectiles  game))
    renderProjectile :: Float -> Color -> Entity -> Picture
    renderProjectile radius col ent =
        uncurry translate (position ent) $ color col $ circleSolid radius

    playerProjectileColor = white
    enemyProjectileColor  = greyN 0.5
    


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
handleKeys (EventKey (SpecialKey KeySpace) state _ _) game = game { keysDown = updatedKeys }
    where
        (w, a, s, d, _) = keysDown game
        updatedKeys = if state == Down then (w, a, s, d, True) else (w, a, s, d, False)
handleKeys (EventKey (Char 'n') _ _ _) game = initialState


-- TODO: after basics: weapon change

-- For a 'p' keypress, pause or unpause the game
handleKeys (EventKey (Char 'p') Down _ _) game =
 game { paused = (not (paused game)) }

handleKeys _ game = game

------------------------------ UPDATES -------------------------------

update :: Float -> ShooterGame -> ShooterGame
update seconds = handleCollisions . runUpdates . moveEntities seconds

-- moves everything
moveEntities :: Float -> ShooterGame -> ShooterGame
moveEntities seconds game = 
    game { 
        enemies = map (moveNonPlayer seconds) (enemies game),
        player = movePlayer seconds (keysDown game) (player game),
        playerProjectiles = map (moveNonPlayer seconds) (playerProjectiles game),
        enemyProjectiles  = map (moveNonPlayer seconds) (enemyProjectiles  game)
        }

runUpdates :: ShooterGame -> ShooterGame
runUpdates game =
    if (not $ alive (player game))
        then initialState
        else
            game { frameCount = (frameCount game) + 1,
                playerProjectiles = newPProjectiles,
                enemyProjectiles =  newEProjectiles,
                enemies = newEnemies
                }
            where
                (x, y) = position $ player game
                (w, a, s, d, space) = keysDown game
                newPProjectiles = if rem (frameCount game) playerFireRate == 0 && space
                                    then (fireProjectile (activeWeapon game) (player game)):(playerProjectiles game)
                                    else playerProjectiles game
                newEProjectiles = if rem (frameCount game) enemyFireRate == 0
                                    then (map (fireProjectile eWeapon1) (enemies game))++(enemyProjectiles game)
                                    else enemyProjectiles game
                newEnemies = if rem ( frameCount game ) enemySpawnRate == 0 
                                then spawnEnemies (enemies game) 
                                else enemies game


handleCollisions :: ShooterGame -> ShooterGame
handleCollisions game = game {
        player = (player game) { health = (health $ player game) - hitsToPlayer}, 
        playerProjectiles = newPProjectiles,
        enemyProjectiles  = newEProjectiles,
        enemies = newEnemies
    }
    where
        newPProjectiles = filter (not . detectCollisionList (enemies game)) (playerProjectiles game)
        newEProjectiles = filter (not . detectCollision (player game)) (enemyProjectiles game)
        newEnemies = filter alive $ map (damageEnemy (playerProjectiles game)) $ filter (not . detectCollision (player game)) (enemies game)
        hitsToPlayer = detectCollisionCount ((enemyProjectiles game) ++ (enemies game)) (player game)
        
alive :: Entity -> Bool
alive ent = (health ent) > 0

damageEnemy :: [Entity] -> Entity -> Entity
damageEnemy projectiles enemy =
    enemy { health = (health enemy) - (detectCollisionCount projectiles enemy) }

-- moves player
movePlayer :: Float -> (Bool, Bool, Bool, Bool, Bool) -> Entity -> Entity
movePlayer seconds (w, a, s, d, sp) ent = ent { position = newPos }
    where
        (xPos, yPos) = position ent
        (xVel, yVel) = evelocity ent
        xPos' = xPos + fromIntegral ( fromEnum d - fromEnum a ) * xVel * seconds
        yPos' = yPos + fromIntegral ( fromEnum w - fromEnum s ) * yVel * seconds
        newPos = putInBounds (xPos', yPos') ( getCircRadius ent )

-- moves all other entities
moveNonPlayer :: Float -> Entity -> Entity
moveNonPlayer seconds ent = ent { position = (xPos', yPos') }
    where
        (xPos, yPos) = position ent
        (xVel, yVel) = evelocity ent
        xPos' = xPos + xVel * seconds
        yPos' = yPos + yVel * seconds

fireProjectile :: Weapon -> Entity -> Entity
fireProjectile weapon ent = Entity (position ent) 
                                   (wvelocity weapon)
                                   (damage weapon)
                                   1
                                   projectileRadius

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
getCircRadius ent = ( sideLength ent / 2 ) / ( 2 * cos ( pi / ( numSides ent ) ) )

-- prevents xPos and yPos from going beyond the screen, use this for the player movement
putInBounds :: (Float, Float) -> Float -> (Float, Float)
putInBounds (xPos, yPos) radius = (xPos', yPos')
    where
        xPos' = min ( max ( xPos ) ( -fromIntegral width  / 2 + radius ) ) ( fromIntegral width  / 2 - radius )
        yPos' = min ( max ( yPos ) ( -fromIntegral height / 2 + radius ) ) ( fromIntegral height / 2 - radius )

spawnEnemies :: [Entity] -> [Entity]
spawnEnemies entList = newEntList
    where
        leftSpawn = -fromIntegral width / 2 + spawnOffset
        rightSpawn = fromIntegral width / 2 - spawnOffset - enemyBaseLength
        spawnDelta = 2 * spawnOffset + enemyBaseLength
        spawnPoints = [ leftSpawn, leftSpawn + 2 * spawnDelta .. rightSpawn ]
        newEntList = entList ++ map spawnEnemy spawnPoints

spawnEnemy :: Float -> Entity
spawnEnemy xCor = Entity (xCor, fromIntegral height / 2 - spawnOffset) (0, -50) 3 4 enemyBaseLength

detectCollisionList :: [Entity] -> Entity -> Bool
detectCollisionList entList entRef = any (detectCollision entRef) entList

detectCollisionCount :: [Entity] -> Entity -> Int
detectCollisionCount entList entRef = length (filter (detectCollision entRef) entList)

detectCollision :: Entity -> Entity -> Bool
detectCollision ent1 ent2 = xCol && yCol
    where
        r1 = getCircRadius ent1
        (xPos1, yPos1) = position ent1
        r2 = getCircRadius ent2
        (xPos2, yPos2) = position ent2
        xCol = (xPos1 + r1) > (xPos2-r2) && (xPos1 - r1) < (xPos2 + r2)
        yCol = (yPos1 + r1) > (yPos2-r2) && (yPos1 - r1) < (yPos2 + r2)


main :: IO ()
main = play window background fps initialState render handleKeys update