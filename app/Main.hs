module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

-- CONTROLS --
-- Move: w-a-s-d
-- Fire: space bar
-- Weapons:
--  h: Standard Projectiles, deal 1 damage
--  j: Exploding Projectiles, use f to detonate to spawn 6 sub-bullets
--  k: Freeze Ray, immobilizes target
-- New Game: n
-- You have 5 health, to restart the game upon death press n.

------------------------------ STATE -------------------------------
-- The player is a triangle
-- The enemy ships are squares
-- All projectiles are circles whose health should be initialized to 1
data Entity = Entity { position :: (Float, Float),
                     evelocity :: (Float, Float),
                     health :: Int,
                     numSides :: Float,
                     sideLength :: Float,
                     effect :: Effect,
                     lastShot :: Int -- Useful for players and enemies only, should be set to a large negative value on start
            } deriving Show

-- Weapons are an ADT, see fireProjectiles for different qualitative functionality across weapon types
data Weapon = EWeapon1 | StandardProj | ExplosiveProj | FreezeProj deriving Show

data Effect = None | Explosive | Freeze | Frozen deriving (Show, Eq)

data ShooterGame = Game
    { 
        frameCount :: Int,
        player :: Entity,
        enemies :: [Entity],
        playerProjectiles :: [Entity],
        enemyProjectiles :: [Entity],
        paused :: Bool,
        keysDown :: (Bool, Bool, Bool, Bool, Bool),
        activeWeapon :: Weapon,
        score :: Int
    } deriving Show

initialState :: ShooterGame
initialState = Game
    {
        frameCount = 0,
        player = Entity (0, -fromIntegral width/2 + fromIntegral offset) (100,100) 5 3 playerSideLength None (-1000),
        enemies = [],
        playerProjectiles = [],
        enemyProjectiles = [],
        paused = False,
        keysDown = (False, False, False, False, False),
        activeWeapon = StandardProj,
        score = 0
    }

-- State upon loss
gameOverState :: Int -> ShooterGame
gameOverState finalScore = Game
    {
        frameCount = 0,
        player = Entity (0, 0) (0,0) 0 3 playerSideLength None (-1000),
        enemies = [],
        playerProjectiles = [],
        enemyProjectiles = [],
        paused = False,
        keysDown = (False, False, False, False, False),
        activeWeapon = StandardProj,
        score = finalScore
    }

-- Window width, height, offset
width, height, offset :: Int
width = 500
height = 500
offset = 50

-- Deprecated, using a cooldown time now
playerFireRate, enemyFireRate, enemySpawnRate :: Int
playerFireRate = 10
enemyFireRate = 100
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
    if (not $ alive (player game))
        then pictures []
    else
        pictures [playerShip,
                    enemyShips,
                    playerProj,
                    enemyProj,
                    displayText
                    ]
        where
            -- text
            displayText = uncurry translate (-fromIntegral width / 2 + 5, fromIntegral height / 2 - 20) 
                $ uncurry scale (0.15, 0.15) 
                $ color white 
                $ Text ( "Lives: " ++ show (health $ player game) ++ " Score: " ++ show (score game) )

            -- TODO: display the current weapon, need to map text strings to actual weapon name
            -- the player
            playerShip = uncurry translate (position $ player game) $ color playerColor $ triangleSolid playerSideLength
            playerColor = dark red

            -- the enemies
            enemyShips = pictures ( map renderEnemy ( enemies game ) )
            renderEnemy :: Entity -> Picture
            renderEnemy ent = 
                uncurry translate (position ent) $ color enemyColor $ rectangleSolid enemyBaseLength enemyBaseLength  

            enemyColor = dark blue -- TODO: fix so it can use multiple enemies

            playerProj = pictures ( map (renderProjectile) (playerProjectiles game))
            enemyProj  = pictures ( map (renderProjectile) (enemyProjectiles  game))

            renderProjectile :: Entity -> Picture
            renderProjectile ent = uncurry translate (position ent) $ color col $ circleSolid radius
                where
                    radius = (sideLength ent)
                    col = case (effect ent) of
                        Explosive -> dark red
                        Freeze -> cyan
                        _ -> white

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
handleKeys (EventKey (Char 'h') _ _ _) game = game { activeWeapon = StandardProj }
handleKeys (EventKey (Char 'j') _ _ _) game = game { activeWeapon = ExplosiveProj }
handleKeys (EventKey (Char 'k') _ _ _) game = game { activeWeapon = FreezeProj }
handleKeys (EventKey (Char 'f') _ _ _) game = game { playerProjectiles = detonateExplosives (playerProjectiles game)}
handleKeys (EventKey (Char 'n') _ _ _) game = initialState


-- TODO: implement this in update function
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
        then gameOverState (score game)
    else
        game { frameCount = (frameCount game) + 1,
            playerProjectiles = newPProjectiles,
            enemyProjectiles =  newEProjectiles,
            enemies = newEnemies,
            player = (player game) { lastShot = newPlayerLastShot }
            }
        where
            playerCooldownTime = (frameCount game) - (lastShot $ player game) -- Time elapsed since last firing of projectile, used with weapons who have a cooldown/reload time
            (x, y) = position $ player game
            (w, a, s, d, space) = keysDown game
            aliveAndInBounds x = (alive x) && (inBounds x)
            newPProjectiles = filter aliveAndInBounds $ 
                                if space
                                    then (fireProjectile (activeWeapon game) (frameCount game) (player game)) ++ (playerProjectiles game)
                                    else playerProjectiles game
            newEProjectiles = filter aliveAndInBounds $
                                if rem (frameCount game) enemyFireRate == 0
                                    then (concatMap (fireProjectile EWeapon1 (frameCount game)) (filter (not . isFrozen) (enemies game))) ++ (enemyProjectiles game)
                                    else enemyProjectiles game
            newEnemies = if rem ( frameCount game ) enemySpawnRate == 120 
                            then spawnEnemies (enemies game) 
                            else enemies game
            newPlayerLastShot = if (length newPProjectiles) > (length $ playerProjectiles game)
                            then frameCount game
                            else (lastShot $ player game)
            isFrozen :: Entity -> Bool
            isFrozen ent = (effect ent) == Frozen

handleCollisions :: ShooterGame -> ShooterGame
handleCollisions game = game {
        player = (player game) { health = (health $ player game) - hitsToPlayer}, 
        playerProjectiles = newPProjectiles,
        enemyProjectiles  = newEProjectiles,
        enemies = newEnemies,
        score = (score game) + killedEnemyCount
    }
    where
        newPProjectiles = filter (not . detectCollisionList (enemies game)) (playerProjectiles game)
        newEProjectiles = filter (not . detectCollision (player game)) (enemyProjectiles game)
        damagedEnemies = map (damageEnemy (playerProjectiles game)) $ filter (not . detectCollision (player game)) (enemies game)
        newEnemies = filter alive $ damagedEnemies
        killedEnemyCount = (length damagedEnemies) - (length newEnemies)
        hitsToPlayer = detectCollisionCount ((enemyProjectiles game) ++ (enemies game)) (player game)

-- Returns true if any of entList has collided with entRef
detectCollisionList :: [Entity] -> Entity -> Bool
detectCollisionList entList entRef = any (detectCollision entRef) entList

-- Returns number of collisions between entRef and any of EntList
detectCollisionCount :: [Entity] -> Entity -> Int
detectCollisionCount entList entRef = length (filter (detectCollision entRef) entList)

-- Returns true if ent1 and ent2 collide
detectCollision :: Entity -> Entity -> Bool
detectCollision ent1 ent2 = xCol && yCol
    where
        r1 = getCircRadius ent1
        (xPos1, yPos1) = position ent1
        r2 = getCircRadius ent2
        (xPos2, yPos2) = position ent2
        xCol = (xPos1 + r1) > (xPos2-r2) && (xPos1 - r1) < (xPos2 + r2)
        yCol = (yPos1 + r1) > (yPos2-r2) && (yPos1 - r1) < (yPos2 + r2)

alive :: Entity -> Bool
alive ent = (health ent) > 0

damageEnemy :: [Entity] -> Entity -> Entity
damageEnemy projectiles enemy =
    enemy { health = (health enemy) - (detectCollisionCount standardProj enemy),
            evelocity = updatedVel,
            effect = updatedEffect }
            where
                standardProj = filter isStandardEntity projectiles
                isStandardEntity :: Entity -> Bool
                isStandardEntity ent = (effect ent) == None
                hitByFreeze ent = ((effect ent) == Freeze) && (detectCollision enemy ent)
                (updatedVel, updatedEffect) = if any hitByFreeze projectiles
                                        then ((0,0), Frozen)
                                        else ((evelocity enemy), (effect enemy))


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

-- Returns a projectile to allow for weapons and enemies to fire multiple at once
fireProjectile :: Weapon -> Int -> Entity -> [Entity]
fireProjectile EWeapon1 _ ent = []
fireProjectile StandardProj currTime ent = 
    if currTime - (lastShot ent) >= 10 -- TODO: change in terms of fps
        then [Entity (position ent) (0, 150) 1 1 projectileRadius None 0] else []
fireProjectile ExplosiveProj currTime ent = 
    if currTime - (lastShot ent) >= fps * 2
        then [Entity (position ent) (0, 90) 1 1 projectileRadius Explosive 0] else []
fireProjectile FreezeProj currTime ent =
    if currTime - (lastShot ent) >= fps
        then [Entity (position ent) (0, 110) 1 1 projectileRadius Freeze 0] else []

-- Will be useful when determining if projectiles should be removed
inBounds :: Entity -> Bool
inBounds ent = (not xOOB) && (not yOOB)
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
spawnEnemy xCor = Entity (xCor, fromIntegral height / 2 - spawnOffset) (0, -50) 1 4 enemyBaseLength None (-1000)

-- Adds and removes necessary projectiles upon detonation (m key)
detonateExplosives :: [Entity] -> [Entity]
detonateExplosives projs = updatedProjectiles
    where 
        isExplosive :: Entity -> Bool
        isExplosive ent = (effect ent) == Explosive

        removedExplosive :: Entity -> Entity
        removedExplosive ent = if (isExplosive ent)
            then ent { health = 0 }
            else ent

        removedExplosives = map removedExplosive projs
        newProjs = concatMap basicSplit (filter isExplosive removedExplosives)
        updatedProjectiles = removedExplosives ++ newProjs


-- Returns four entities each with velocity 45 degrees from axes (all four directions diagonally)
basicSplit :: Entity -> [Entity]
basicSplit ent = 
    [(Entity (xPos, yPos) (diag, diag) 1 1 projectileRadius None 0), 
    (Entity (xPos, yPos) (-diag, diag) 1 1 projectileRadius None 0),
    (Entity (xPos, yPos) (diag, -diag) 1 1 projectileRadius None 0),
    (Entity (xPos, yPos) (-diag, -diag) 1 1 projectileRadius None 0),
    (Entity (xPos, yPos) (yVel, 0) 1 1 projectileRadius None 0),
    (Entity (xPos, yPos) (-yVel, 0) 1 1 projectileRadius None 0)]
    where
        (xPos, yPos) = (position ent)
        (_, yVel) = (evelocity ent)
        diag = yVel * (1/(sqrt 2))


main :: IO ()
main = play window background fps initialState render handleKeys update