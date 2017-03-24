module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap()
import Graphics.Gloss.Data.Picture()
import System.IO

import Prelude hiding (Either(..))
import Data.List (sort, delete, unfoldr)
import Control.Monad (forM_, liftM)
import System.IO.Unsafe

window :: Display
window = InWindow "Sokoban" (1200, 800) (50, 30)

background :: Color
background = white

fps :: Int
fps = 60

update :: Float -> Game -> Game
update _ game = game
--{time = liftIO getTicks}

--drawing :: Picture -> Picture
--drawing bitMap = pictures[bitMap, translate (-60) (-60) bitMap, translate 60 60 $ scale 2 2 bitMap]

emptyMap :: SokobanMap
emptyMap  = SokobanMap
         {size        = 50
         ,sizeX       = 0
         ,sizeY       = 0
         ,walls       = []
         ,targets     = []
         ,boxes       = []
         ,darkboxes   = []
         ,spaces      = []
         ,player      = (0,0)
         ,previousMap = Nothing
         }


elemSize :: Int
elemSize = 60   --all element images are 60*60 pixels





--(mapNumber:steps:(walls mapStruct):(targets mapStruct):(boxes mapStruct):(darkboxes mapStruct):(spaces mapStruct):(player mapStruct):[])


makeMap :: String -> SokobanMap
makeMap str = snd (foldl check ((0, 0), emptyMap) str)
      where check ((i, j), mapStruct) symbol
                | symbol == '\n' = ((i + 1, 0), mapStruct{sizeY = max j (sizeY mapStruct),
                                                          sizeX = max (i + 2) (sizeX mapStruct)})
                | otherwise = ((i, j + 1), addSymbol mapStruct{sizeY = max (j + 2) (sizeY mapStruct)} symbol (i, j))

addSymbol :: SokobanMap -> Char -> Coord -> SokobanMap
addSymbol mapStruct symbol coord =
    case symbol of
          ' ' -> mapStruct
          '@' -> mapStruct{ player  = coord }
          'x' -> mapStruct{ boxes   = coord : boxes mapStruct }
          '#' -> mapStruct{ walls   = coord : walls mapStruct }
          '*' -> mapStruct{ targets = coord : targets mapStruct }
          '$' -> mapStruct{ spaces  = coord : spaces mapStruct }
          _ -> error (show symbol ++ " not recognized")

makeLevels :: [String] -> [SokobanMap]
--makeLevels maps = zipWith makeMap maps mapSizes
makeLevels levels = map makeMap levels


loadLevels :: String -> String -> [String]
loadLevels [] [] = []
loadLevels [] _ = []
loadLevels (x : xs) buf
      | x == '\n' && (length buf) == 0 || x == '{'  = (loadLevels xs buf)
      | x == '}'   = buf : (loadLevels xs [])
      | otherwise  = loadLevels xs (buf ++ x : [])

makeGame :: MapTextures -> Menu -> [SokobanMap] -> Int -> Int -> Game
makeGame  mapTextures menuStruct maps num step_num  =
                            Game {sokobanMaps = maps
                                 ,currMap     = maps !! 0
                                 ,backupMap   = maps !! 0
                                 ,savedMaps   = []
                                 ,currNumber  = num
                                 ,menu        = menuStruct
                                 ,state       = 0
                                 ,scaleAll    = 1.0
															   ,steps		    = step_num
															   ,labelSteps  = show_steps step_num
                                 ,textures    = mapTextures
															   }

makeMenu :: Picture -> Menu
makeMenu menuBg =       Menu {menuBackground   = menuBg
                             ,labelHeader      = translate (-60)     150 $ scale 0.4 0.4 $ color white $ text "Menu"
                             ,labelScaleInc    = translate (-200)     80 $ scale 0.2 0.2 $ color white $ text "Press '+' to upscale"
                             ,labelScaleDec    = translate (-200)     30 $ scale 0.2 0.2 $ color white $ text "Press '-' to downscale"
                             ,labelRestart     = translate (-200)  (-20) $ scale 0.2 0.2 $ color white $ text "Press 'r' to restart level"
                             ,labelRestartGame = translate (-200)  (-70) $ scale 0.2 0.2 $ color white $ text "Press 'n' to restart game"
                             ,labelPrevState   = translate (-200) (-120) $ scale 0.2 0.2 $ color white $ text "Press 'p' to undo last step"
                             }

makeSavesMenu :: Picture -> SavesMenu
makeSavesMenu savesBg = SavesMenu
                        {savesBackground = savesBg
                        ,labelHeader = translate (-60) 150 $ scale 0.4 0.4 $ color white $ text "Saves"
                        ,savesList = []
                        ,numSaves = 0
                        }

render :: Game -> Picture
-- the sequence of elements in the list (which is the argument for function pictures) shows
-- how elements will be rendered
render game
    | st   == 1 = pictures [bg , foldMap (\f -> f menuStruct) [menuBackground, labelHeader, labelScaleInc, labelScaleDec, labelRestart, labelRestartGame, labelPrevState]]

    | otherwise = pictures [bg, foldMap pictures picturesList, pictures playerObj, steps]
     where
            st         = state game
            menuStruct = menu game
            savesStruct= savesMenu game
            mapStruct  = currMap game
            num        = currNumber game
            scAll      = scaleAll game
            objsTexture= textures game
            steps      = labelSteps game

            x          = sizeX mapStruct
            y          = sizeY mapStruct
            currSize   = size mapStruct
            resize     = (convert currSize) / (convert elemSize) * scAll  --koeff
            bg         = (bgTexture objsTexture)

            objectsList  = map (\f -> f mapStruct) [walls, targets, boxes, darkboxes]
            texturesList = map (\f -> f objsTexture) [wallTexture, targetTexture, boxTexture, darkboxTexture]
            picturesList = map (\(object, texture) -> makeObjs object texture x  y resize) (zip objectsList texturesList)
            playerObj    = makeObjs ((player mapStruct) : [])  (playerTexture  objsTexture) x y resize

show_steps :: Int -> Picture
show_steps step_num = translate (-550)  280 $ scale 0.4 0.4 $ color white $ text ("Steps: " ++ (show step_num))

convert :: Int -> Float
convert x = fromIntegral (x :: Int) :: Float

makeObjs :: [Coord] -> Picture -> Int -> Int -> Float -> [Picture]
makeObjs [] _ _ _ _= []
makeObjs ((i, j) : xs) pic sizeX sizeY koeff = (translate  x y $ scale koeff koeff pic) : makeObjs xs pic sizeX sizeY koeff
          where
            x = (convert (-(sizeY `div` 2) * elemSize + j * elemSize)) * koeff
            y = (convert ( (sizeX `div` 2) * elemSize - i * elemSize)) * koeff







showMenu :: Game -> Game
showMenu game
  | st == 0 = game {state = 1}
  | otherwise = game {state = 0}
  where st = (state game)

loadNextLevel :: Game -> Game
loadNextLevel game = game {currMap = newMap, backupMap = newMap, currNumber = num, steps = 0,labelSteps  = show_steps 0}
                      where
                        maps = (sokobanMaps game)
                        num = ((currNumber game) + 1) `mod` (length maps)
                        newMap = maps !! num

startFromBegin :: Game -> Game
startFromBegin game = game {currMap     = firstMap
                           ,backupMap   = firstMap
                           ,currNumber  = 0
					                 ,steps       = 0
						               ,labelSteps  = show_steps 0
                           }
                      where
                        maps = (sokobanMaps game)
                        firstMap = maps !! 0

reloadLevel :: Game -> Game
reloadLevel game = game {currMap = savedMap, steps = 0 ,labelSteps  = show_steps 0}
                   where
                     savedMap = (backupMap game)

saveGame :: Game -> Game
saveGame game = unsafePerformIO (func game)

prepare :: [String] -> String -> String
prepare levels str
    | (length levels) < 9 = (foldr (\s acc -> s ++ acc) "" levels) ++ str
    | otherwise = (foldr (\s acc -> s ++ acc) "" (tail levels)) ++ str

func :: Game -> IO Game
func game = do
        str <-  return (makeString (currMap game) (currNumber game) (steps game))
        --saved <- readFile "output.txt"
        --maps <- return (loadLevels saved [])
        --updated <- return (prepare maps str)

        --writeFile "output.txt" updated
        return game{savedMaps = (savedMaps game) ++ ((currMap game){mapNumber = (currNumber game)}) : []}
--game {savedMaps = (savedMaps game) ++ (currMap game) : []}

loadGame :: Game -> Int -> Game
loadGame game num
    | num >= 1 && num <= 9 && num <= (length maps) =
                            game{currMap = newMap{previousMap = Nothing}
                                ,currNumber = (mapNumber newMap)
                                ,labelSteps  = show_steps 0
                                ,steps = 0
                                ,backupMap = newMap
                                }
    | otherwise = game
    where
      maps = savedMaps game
      newMap = (maps !! (num - 1))

makeString :: SokobanMap -> Int -> Int -> String
makeString mapStruct mapNumber steps=
   "{" ++ (show [mapNumber, steps]) ++ (show ((walls mapStruct):(targets mapStruct):(boxes mapStruct):(darkboxes mapStruct):(spaces mapStruct):((player mapStruct):[]):[])) ++ "}"


makeStepBack :: Game -> Game
makeStepBack game = game {currMap = savedState, steps = updatedSteps , labelSteps  = show_steps updatedSteps}
                    where
                      savedState = case (previousMap (currMap game)) of
                        Nothing -> (currMap game)
                        Just state -> state
                      updatedSteps
                                | (steps game) > 0 = (steps game) - 1
                                | otherwise = 0

scaleIn :: Game -> Game
scaleIn game = game {scaleAll = scAll + 0.1}
               where
                 scAll = (scaleAll game)


scaleOut :: Game -> Game
scaleOut game = game {scaleAll = scAll - 0.1}
               where
                 scAll = (scaleAll game)

makeMove :: (Int, Int) -> Game -> Game
makeMove (di, dj) game = action game (i + di, j + dj) (i + 2*di, j + 2*dj)
                         where
                           mapStruct = (currMap game)
                           i = fst (player mapStruct)
                           j = snd (player mapStruct)

moveDown :: Game -> Game
moveDown = makeMove (1, 0)

moveUp :: Game -> Game
moveUp = makeMove (-1, 0)

moveLeft :: Game -> Game
moveLeft = makeMove (0, -1)

moveRight :: Game -> Game
moveRight = makeMove (0, 1)

-- | Respond to key events.
handleKeys :: Event -> Game -> Game
handleKeys (EventKey (Char c) Down _ _ )game = case c of
  'm' -> showMenu game
  'u' -> saveGame game
  '.' -> loadNextLevel game
  'n' -> startFromBegin game
  'r' -> reloadLevel game
  'p' -> makeStepBack game
  '=' -> scaleIn game
  '-' -> scaleOut game
  's' -> moveDown game
  'w' -> moveUp game
  'a' -> moveLeft game
  'd' -> moveRight game

  '1' -> loadGame game 1
  '2' -> loadGame game 2
  '3' -> loadGame game 3
  '4' -> loadGame game 4
  '5' -> loadGame game 5
  '6' -> loadGame game 6
  '7' -> loadGame game 7
  '8' -> loadGame game 8
  '9' -> loadGame game 9
  -- Do nothing for all other events.
  _ -> game
handleKeys _ game = game

checkWin :: Game -> Game
checkWin game
        | length boxesCoords == 0 = game{currNumber  = nextNumber
                                        ,currMap     = newMap
                                        ,backupMap   = newMap
                                        ,steps = 0
                                        ,labelSteps  = show_steps 0
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
                                                ,boxes = if (isObject followCoord targetsCoords) then (deleteBox newCoord boxesCoords)
                                                         else (moveBox newCoord followCoord boxesCoords)
                                                ,darkboxes = if (isObject followCoord targetsCoords) then followCoord : darkboxesCoords
                                                             else darkboxesCoords
                                                ,previousMap = Just mapStruct
                                                }
                          ,steps = stepNumber + 1
												  ,labelSteps  = show_steps (stepNumber + 1)
                          }

        | (isObject newCoord darkboxesCoords) && (not (isObject followCoord wallsCoords)) =
            checkWin game {currMap = mapStruct {player = newCoord
                                               ,boxes = if (isObject followCoord targetsCoords) then boxesCoords
                                                        else followCoord : boxesCoords
                                               ,darkboxes = if (isObject followCoord targetsCoords) then (moveBox newCoord followCoord darkboxesCoords)
                                                            else (deleteBox newCoord darkboxesCoords)
                                               ,previousMap = Just mapStruct
                                               }
											   ,steps = stepNumber + 1
											   ,labelSteps  = show_steps (stepNumber + 1)
                          }



        | otherwise = game {currMap = mapStruct {player = newCoord
                                                ,previousMap = Just mapStruct
                                                }
												,steps = stepNumber + 1
												,labelSteps  = show_steps (stepNumber + 1)
                           }
        where
           mapStruct        = (currMap game)
           wallsCoords      = (walls   mapStruct)
           targetsCoords    = (targets mapStruct)
           boxesCoords      = (boxes   mapStruct)
           stepNumber       = (steps   game)
           darkboxesCoords  = (darkboxes mapStruct)


isObject :: Coord -> [Coord] -> Bool
isObject (i, j) objects = foldr (\(a, b) acc -> (i == a && j == b) || acc) False objects

moveBox :: Coord -> Coord -> [Coord] -> [Coord]
moveBox (i, j) (newI, newJ) _boxes = map (\(a, b) -> if (i == a && j == b) then (newI, newJ) else (a, b)) _boxes

deleteBox :: Coord -> [Coord] -> [Coord]
deleteBox (i, j) _boxes = filter (\(a, b) -> not (i == a && j == b)) _boxes



main :: IO()
main = do
     src <- readFile "levels.txt"
     levelsData <- return (loadLevels src [])   -- converts any object to IO object

     box     <- loadBMP "./images/box.bmp"
     darkbox <- loadBMP "./images/darkbox.bmp"
     wall    <- loadBMP "./images/wall.bmp"
     space   <- loadBMP "./images/space.bmp"
     target  <- loadBMP "./images/target.bmp"
     _player  <- loadBMP "./images/player.bmp"
     bg      <- loadBMP "./images/background3.bmp"

     levels  <- return (makeLevels levelsData)
     textures <- return MapTextures
                  {boxTexture = box
                  ,darkboxTexture = darkbox
                  ,wallTexture = wall
                  ,spaceTexture = space
                  ,targetTexture = target
                  ,playerTexture = _player
                  ,bgTexture = bg
                  }

     menuBg <- loadBMP "./images/menuBg.bmp"
     savesBg <- loadBMP "./images/savesBg.bmp"
     _menu <- return (makeMenu menuBg)
     _saves <- return (makeSavesMenu savesBg)
     game <- return (makeGame textures _menu levels 0 0)
     play window background fps game render handleKeys update
     return ()
