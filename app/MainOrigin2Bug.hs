module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Data.Picture

{-}
-- | Play a game in a window.
play :: Display -- ^ Window to draw game in.
     -> Color   -- ^ Background color.
     -> Int     -- ^ Number of simulation steps per second of real time.
     -> a       -- ^ The initial game state.
     -> (a -> Picture)       -- ^ A function to render the world a picture.
     -> (Event -> a -> a)    -- ^ A function to handle input events.
     -> (Float -> a -> a)    -- ^ A function to step the world one iteration.
     -> IO ()
-}

window :: Display
window = InWindow "Sokoban" (1200, 800) (50, 30)

background :: Color
background = white

fps :: Int
fps = 60

update :: Float -> Game -> Game
update _ game = game

--drawing :: Picture -> Picture
--drawing bitMap = pictures[bitMap, translate (-60) (-60) bitMap, translate 60 60 $ scale 2 2 bitMap]


type Coord = (Int, Int)

emptyMap = SokobanMap
                   {size      = 50
                   ,walls     = []
                   ,targets   = []
                   ,boxes     = []
                   ,darkboxes = []
                   ,spaces    = []
                   ,player    = (0,0)
                   ,steps     = 0
                   }




mapSizes :: [(Int, Int)]
mapSizes = [ (9, 8),
             (8, 14),
             (10, 17)]

elemSize :: Int
elemSize = 60   --all element images are 60*60 pixels







makeMap :: MapTextures -> String -> (Int, Int) -> SokobanMap
makeMap mapTextures str size = snd (foldl check ((0, 0), emptyMap{textures = mapTextures}) str)
      where check ((i, j), mapStruct) symbol
                | j == snd size - 1 = ((i + 1, 0), addSymbol mapStruct symbol (i, j))
                | otherwise = ((i, j + 1), addSymbol mapStruct symbol (i, j))

addSymbol :: SokobanMap -> Char -> Coord -> SokobanMap
addSymbol mapStruct symbol coord =
    case symbol of
          '@' -> mapStruct{ player  = coord }
          'x' -> mapStruct{ boxes   = coord : boxes mapStruct }
          '#' -> mapStruct{ walls   = coord : walls mapStruct }
          '*' -> mapStruct{ targets = coord : targets mapStruct }
          '$' -> mapStruct{ spaces  = coord : spaces mapStruct }
          otherwise -> error (show symbol ++ " not recognized")

makeLevels :: [String] -> MapTextures -> [SokobanMap]
makeLevels maps textures = zipWith (makeMap textures) maps mapSizes

loadLevels :: String -> String -> [String]
loadLevels [] [] = []
loadLevels [] _ = []
loadLevels (x : xs) buf
      | x == '{' || x == '\n' = (loadLevels xs buf)
      | x == '}'              = buf : (loadLevels xs [])
      | otherwise             = loadLevels xs (buf ++ x : [])

makeGame :: Menu -> [SokobanMap] -> SokobanMap -> SokobanMap -> SokobanMap -> Int -> Game
makeGame menuStruct maps curr savedMap savedState num = Game {sokobanMaps = maps
                                                             ,currMap     = curr
                                                             ,backupMap   = savedMap
                                                             ,stateBackup = savedState
                                                             ,currNumber  = num
                                                             ,menu        = menuStruct
                                                             ,state       = 0
                                                             ,scaleAll    = 1.0
                                                             }

makeMenu :: Picture -> Picture -> Menu
makeMenu menuBg allBg = Menu {menuBackground   = menuBg
                             ,labelHeader      = translate (-60)     150 $ scale 0.4 0.4 $ color white $ text "Menu"
                             ,labelScaleInc    = translate (-200)     80 $ scale 0.2 0.2 $ color white $ text "Press '+' to upscale"
                             ,labelScaleDec    = translate (-200)     30 $ scale 0.2 0.2 $ color white $ text "Press '-' to downscale"
                             ,labelRestart     = translate (-200)  (-20) $ scale 0.2 0.2 $ color white $ text "Press 'r' to restart level"
                             ,labelRestartGame = translate (-200)  (-70) $ scale 0.2 0.2 $ color white $ text "Press 'n' to restart game"
                             ,labelPrevState   = translate (-200) (-120) $ scale 0.2 0.2 $ color white $ text "Press 'p' to undo last step"
                             }



render :: Game -> Picture
-- the sequence of elements in the list (which is the argument for function pictures) shows
-- how elements will be rendered
render game
    | st   == 1 = pictures [bg , foldMap (\f -> f menuStruct) [menuBackground, labelHeader, labelScaleInc, labelScaleDec, labelRestart, labelRestartGame, labelPrevState]]
    | otherwise = pictures (map pictures [bg : [], wallObjs, targetObjs, boxObjs, darkboxObjs, playerObj])
     where
            st         = (state game)
            menuStruct = (menu game)

            mapStruct  = (currMap game)
            num        = (currNumber game)
            sizeX      = fst (mapSizes !! num)
            sizeY      = snd (mapSizes !! num)
            scAll      = (scaleAll game)
            currSize   = (size mapStruct)
            resize     = (fromIntegral currSize) / (fromIntegral elemSize) * scAll  --koeff
            bg         = bgTexture (textures mapStruct)
            objsTexture= (textures mapStruct)

  --foldMap (\(getObjs, getTexture) -> ...) [(walls, wallTexture), ...]

            wallObjs   = makeObjs (walls mapStruct)          (wallTexture    objsTexture) sizeX sizeY resize
            targetObjs = makeObjs (targets mapStruct)        (targetTexture  objsTexture) sizeX sizeY resize
            boxObjs    = makeObjs (boxes mapStruct)          (boxTexture     objsTexture) sizeX sizeY resize
            darkboxObjs= makeObjs (darkboxes mapStruct)      (darkboxTexture objsTexture) sizeX sizeY resize
            playerObj  = makeObjs ((player mapStruct) : [])  (playerTexture  objsTexture) sizeX sizeY resize

makeObjs :: [Coord] -> Picture -> Int -> Int -> Float -> [Picture]
makeObjs [] _ _ _ _= []
makeObjs ((i, j) : xs) pic sizeX sizeY koeff = (translate  x y $ scale koeff koeff pic) : makeObjs xs pic sizeX sizeY koeff
          where
            x = (fromIntegral (-(sizeY `div` 2) * elemSize + j * elemSize)) * koeff
            y = (fromIntegral ( (sizeX `div` 2) * elemSize - i * elemSize)) * koeff







-- | Respond to key events.
handleKeys :: Event -> Game -> Game

handleKeys (EventKey (Char 'm') Down _ _) game
        | st == 0 = game {state = 1}             --open menu
        | otherwise = game {state = 0}           --close menu
         where st = (state game)

handleKeys (EventKey (Char '.') Down _ _) game = game {currMap = newMap, backupMap = newMap, stateBackup = newMap, currNumber = num}
                                                  where
                                                    maps = (sokobanMaps game)
                                                    num = ((currNumber game) + 1) `mod` (length maps)
                                                    newMap = maps !! num

handleKeys (EventKey (Char 'n') Down _ _) game = game {currMap     = firstMap
                                                      ,backupMap   = firstMap
                                                      ,stateBackup = firstMap
                                                      ,currNumber  = 0
                                                      }
                                                  where
                                                    maps = (sokobanMaps game)
                                                    firstMap = maps !! 0

handleKeys (EventKey (Char 'r') Down _ _) game = game {currMap = savedMap, stateBackup = savedMap}
                                                  where
                                                    savedMap = (backupMap game)

handleKeys (EventKey (Char 'p') Down _ _) game = game {currMap = savedState}
                                                  where
                                                    savedState = (stateBackup game)


handleKeys (EventKey (Char '=') Down _ _) game = game {scaleAll = scAll + 0.1}
                                                  where
                                                    scAll = (scaleAll game)


handleKeys (EventKey (Char '-') Down _ _) game = game {scaleAll = scAll - 0.1}
                                                  where
                                                    scAll = (scaleAll game)

handleKeys (EventKey (Char 's') Down _ _) game = action game (i + 1, j) (i + 2, j)
                                                        where
                                                            mapStruct = (currMap game)
                                                            i = fst (player mapStruct)
                                                            j = snd (player mapStruct)

handleKeys (EventKey (Char 'w') Down _ _) game = action game (i - 1, j) (i - 2, j)
                                                        where
                                                            mapStruct = (currMap game)
                                                            i = fst (player mapStruct)
                                                            j = snd (player mapStruct)

handleKeys (EventKey (Char 'a') Down _ _) game = action game (i, j - 1) (i, j - 2)
                                                        where
                                                            mapStruct = (currMap game)
                                                            i = fst (player mapStruct)
                                                            j = snd (player mapStruct)

handleKeys (EventKey (Char 'd') Down _ _) game = action game (i, j + 1) (i, j + 2)
                                                        where
                                                            mapStruct = (currMap game)
                                                            i = fst (player mapStruct)
                                                            j = snd (player mapStruct)

-- Do nothing for all other events.
handleKeys _ game = game

checkWin :: Game -> Game
checkWin game
        | length boxesCoords == 0 = game{currNumber  = nextNumber
                                        ,currMap     = newMap
                                        ,backupMap   = newMap
                                        ,stateBackup = newMap
                                        }
        | otherwise = game
        where
          num         = (currNumber game)
          mapStruct   = (currMap game)
          maps        = (sokobanMaps game)
          boxesCoords = (boxes mapStruct)
          nextNumber  = (num + 1) `mod` (length maps)

          newMap      = maps !! nextNumber

action :: Game -> Coord -> Coord -> Game
action game newCoord followCoord
        | (isObject newCoord wallsCoords) ||
          ((isObject newCoord boxesCoords) || (isObject newCoord darkboxesCoords)) && ((isObject followCoord boxesCoords) || (isObject followCoord darkboxesCoords)) ||
          ((isObject newCoord boxesCoords) || (isObject newCoord darkboxesCoords)) && (isObject followCoord wallsCoords)  = game

        | (isObject newCoord boxesCoords) && (not (isObject followCoord wallsCoords)) =
            checkWin game {currMap = mapStruct {player = newCoord
                                                ,steps = stepNumber + 1
                                                ,boxes = if (isObject followCoord targetsCoords) then (deleteBox newCoord boxesCoords)
                                                         else (moveBox newCoord followCoord boxesCoords)
                                                ,darkboxes = if (isObject followCoord targetsCoords) then followCoord : darkboxesCoords
                                                             else darkboxesCoords
                                                }
                          ,stateBackup = mapStruct
                          }

        | (isObject newCoord darkboxesCoords) && (not (isObject followCoord wallsCoords)) =
            checkWin game {currMap = mapStruct {player = newCoord
                                               ,steps = stepNumber + 1
                                               ,boxes = if (isObject followCoord targetsCoords) then boxesCoords
                                                        else followCoord : boxesCoords
                                               ,darkboxes = if (isObject followCoord targetsCoords) then (moveBox newCoord followCoord darkboxesCoords)
                                                            else (deleteBox newCoord darkboxesCoords)
                                               }
                          ,stateBackup = mapStruct
                          }



        | otherwise = game {currMap = mapStruct {player = newCoord
                                                ,steps = stepNumber + 1
                                                }
                           ,stateBackup = mapStruct
                           }
        where
           mapStruct        = (currMap game)
           wallsCoords      = (walls   mapStruct)
           targetsCoords    = (targets mapStruct)
           boxesCoords      = (boxes   mapStruct)
           stepNumber       = (steps   mapStruct)
           darkboxesCoords  = (darkboxes mapStruct)


isObject :: Coord -> [Coord] -> Bool
isObject (i, j) objects = foldr (\(a, b) acc -> (i == a && j == b) || acc) False objects

moveBox :: Coord -> Coord -> [Coord] -> [Coord]
moveBox (i, j) (newI, newJ) boxes = map (\(a, b) -> if (i == a && j == b) then (newI, newJ) else (a, b)) boxes

deleteBox :: Coord -> [Coord] -> [Coord]
deleteBox (i, j) boxes = filter (\(a, b) -> not (i == a && j == b)) boxes






main :: IO()
main = do
     src <- readFile "levels.txt"
     levelsData <- return (loadLevels src [])   -- converts any object to IO object

     box     <- loadBMP "./images/box.bmp"
     darkbox <- loadBMP "./images/darkbox.bmp"
     wall    <- loadBMP "./images/wall.bmp"
     space   <- loadBMP "./images/space.bmp"
     target  <- loadBMP "./images/target.bmp"
     player  <- loadBMP "./images/player.bmp"
     bg      <- loadBMP "./images/background3.bmp"

     levels  <- return (makeLevels levelsData
                                  MapTextures {boxTexture = box
                                               ,darkboxTexture = darkbox
                                               ,wallTexture = wall
                                               ,spaceTexture = space
                                               ,targetTexture = target
                                               ,playerTexture = player
                                               ,bgTexture = bg})

     menuBg <- loadBMP "./images/menuBg.bmp"

     menu <- return (makeMenu menuBg bg)
     game <- return (makeGame menu levels (levels !! 0) (levels !! 0) (levels !! 0) 0)
     play window background fps game render handleKeys update

     return ()
