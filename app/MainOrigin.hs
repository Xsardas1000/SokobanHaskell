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

update :: Float -> SokobanMap -> SokobanMap
update _ mapStruct = mapStruct

--drawing :: Picture -> Picture
--drawing bitMap = pictures[bitMap, translate (-60) (-60) bitMap, translate 60 60 $ scale 2 2 bitMap]


type Coord = (Int, Int)
data SokobanMap = SokobanMap
                        {size      :: Int
                        ,walls     :: [Coord]
                        ,targets   :: [Coord]
                        ,boxes     :: [Coord]
                        ,darkboxes :: [Coord]
                        ,spaces    :: [Coord]
                        ,player    :: Coord
                        ,steps     :: Int
                        ,textures  :: MapTextures
                        } deriving Show

emptyMap = SokobanMap
                   {size      = 60
                   ,walls     = []
                   ,targets   = []
                   ,boxes     = []
                   ,darkboxes = []
                   ,spaces    = []
                   ,player    = (0,0)
                   ,steps     = 0
                   }

data MapTextures = MapTextures
                  {bgTexture      :: Picture
                  ,wallTexture    :: Picture
                  ,spaceTexture   :: Picture
                  ,boxTexture     :: Picture
                  ,darkboxTexture :: Picture
                  ,targetTexture  :: Picture
                  ,playerTexture  :: Picture
                  } deriving Show


mapSizes :: [(Int, Int)]
mapSizes = [ (9, 8),
             (9, 8) ]

elemSize :: Int
elemSize = 60







makeMap :: String -> MapTextures ->Int -> SokobanMap
makeMap str mapTextures num = snd (foldl check ((0, 0), emptyMap{textures = mapTextures}) str)
      where check ((i, j), mapStruct) symbol
                | (j == (snd (mapSizes !! num)) - 1) = ((i + 1, 0), addSymbol mapStruct symbol (i, j))
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

makeLevels :: [String] -> MapTextures -> Int -> [SokobanMap]
makeLevels [] mapTextures num = []
makeLevels (x : xs) mapTextures num = (makeMap x mapTextures num) : (makeLevels xs mapTextures (num + 1))

loadLevels :: String -> String -> [String]
loadLevels [] [] = []
loadLevels [] _ = []
loadLevels (x : xs) buf
      | x == '{' || x == '\n' = (loadLevels xs buf)
      | x == '}'              = buf : (loadLevels xs [])
      | otherwise             = loadLevels xs (buf ++ x : [])







render :: SokobanMap -> Picture
-- the sequence of elements in the list (which is the argument for function pictures) shows
-- how elements will be rendered
render mapStruct = pictures (map pictures [bg, space, wallObjs, targetObjs, boxObjs, darkboxObjs, playerObj])
         where
            bg         = (bgTexture (textures mapStruct)) :[]
            space      = (translate ((convert (-elemSize)) * 0.5) (convert (-elemSize + 80)) (spaceTexture (textures mapStruct))) :[]
            wallObjs   = makeObjs (walls mapStruct)          (wallTexture    (textures mapStruct))
            targetObjs = makeObjs (targets mapStruct)        (targetTexture  (textures mapStruct))
            boxObjs    = makeObjs (boxes mapStruct)          (boxTexture     (textures mapStruct))
            darkboxObjs= makeObjs (darkboxes mapStruct)      (darkboxTexture (textures mapStruct))
            playerObj  = makeObjs ((player mapStruct) : [])  (playerTexture  (textures mapStruct))

convert :: Int -> Float
convert x = fromIntegral (x :: Int) :: Float

makeObjs :: [Coord] -> Picture -> [Picture]
makeObjs [] _ = []
makeObjs ((i, j) : xs) pic = (translate (convert (-4 * elemSize + j * elemSize)) (convert (80 + 3 * elemSize + (-i * elemSize))) pic) : makeObjs xs pic







-- | Respond to key events.
handleKeys :: Event -> SokobanMap -> SokobanMap

handleKeys (EventKey (Char 's') Down _ _) mapStruct = action mapStruct (i + 1, j) (i + 2, j)
                                                        where
                                                            i = fst (player mapStruct)
                                                            j = snd (player mapStruct)

handleKeys (EventKey (Char 'w') Down _ _) mapStruct = action mapStruct (i - 1, j) (i - 2, j)
                                                        where
                                                            i = fst (player mapStruct)
                                                            j = snd (player mapStruct)

handleKeys (EventKey (Char 'a') Down _ _) mapStruct = action mapStruct (i, j - 1) (i, j - 2)
                                                        where
                                                            i = fst (player mapStruct)
                                                            j = snd (player mapStruct)

handleKeys (EventKey (Char 'd') Down _ _) mapStruct = action mapStruct (i, j + 1) (i, j + 2)
                                                        where
                                                            i = fst (player mapStruct)
                                                            j = snd (player mapStruct)

-- Do nothing for all other events.
handleKeys _ mapStruct = mapStruct






action :: SokobanMap -> Coord -> Coord -> SokobanMap
action mapStruct newCoord followCoord
        | (isWall newCoord wallsCoords) ||
          ((isBox newCoord boxesCoords) || (isDarkbox newCoord darkboxesCoords)) && ((isBox followCoord boxesCoords) || (isDarkbox followCoord darkboxesCoords)) ||
          ((isBox newCoord boxesCoords) || (isDarkbox newCoord darkboxesCoords)) && (isWall followCoord wallsCoords)  = mapStruct

        | (isBox newCoord boxesCoords) && (not (isWall followCoord wallsCoords)) =
                mapStruct{player = newCoord
                         ,steps = stepNumber + 1
                         ,boxes = if (isTarget followCoord targetsCoords) then (deleteBox newCoord boxesCoords)
                                  else (moveBox newCoord followCoord boxesCoords)
                         ,darkboxes = if (isTarget followCoord targetsCoords) then followCoord : darkboxesCoords
                                      else darkboxesCoords
                         }

        | (isDarkbox newCoord darkboxesCoords) && (not (isWall followCoord wallsCoords)) =
                mapStruct{player = newCoord
                         ,steps = stepNumber + 1
                         ,boxes = if (isTarget followCoord targetsCoords) then boxesCoords
                                  else followCoord : boxesCoords
                         ,darkboxes = if (isTarget followCoord targetsCoords) then (moveBox newCoord followCoord darkboxesCoords)
                                      else (deleteBox newCoord darkboxesCoords)
                         }


        | otherwise = mapStruct{player = newCoord, steps = stepNumber + 1}
        where
           wallsCoords      = (walls   mapStruct)
           targetsCoords    = (targets mapStruct)
           boxesCoords      = (boxes   mapStruct)
           stepNumber       = (steps   mapStruct)
           darkboxesCoords  = (darkboxes mapStruct)



isWall :: Coord -> [Coord] -> Bool
isWall (i, j) walls = not (foldr1 (&&) (map (\(a, b) ->
                                              if (i == a && j == b) then False else True) walls))

isTarget :: Coord -> [Coord] -> Bool
isTarget (i, j) targets = not (foldr1 (&&) (map (\(a, b) ->
                                              if (i == a && j == b) then False else True) targets))

isBox :: Coord -> [Coord] -> Bool
isBox (i, j) boxes = not (foldr(\(a, b) acc -> (not (i == a && j == b)) && acc) True boxes)

isDarkbox :: Coord -> [Coord] -> Bool
isDarkbox (i, j) darkboxes = not (foldr(\(a, b) acc -> (not (i == a && j == b)) && acc) True darkboxes)

moveBox :: Coord -> Coord -> [Coord] -> [Coord]
moveBox (i, j) (newI, newJ) boxes = map (\(a, b) ->
                                              if (i == a && j == b) then (newI, newJ) else (a, b)) boxes

deleteBox :: Coord -> [Coord] -> [Coord]
deleteBox (i, j) boxes = filter (\(a, b) -> not (i == a && j == b)) boxes




main :: IO()
main = do
     src <- readFile "levels.txt"
     levelsData <- return (loadLevels src [])   -- converts any object to IO object

     box <- loadBMP "./images/box.bmp"
     darkbox <- loadBMP "./images/darkbox.bmp"
     wall <- loadBMP "./images/wall.bmp"
     space <- loadBMP "./images/space.bmp"
     target <- loadBMP "./images/target.bmp"
     player <- loadBMP "./images/player.bmp"
     bg <- loadBMP "./images/background3.bmp"

     levels <- return (makeLevels levelsData
                                  MapTextures{boxTexture = box
                                              ,darkboxTexture = darkbox
                                              ,wallTexture = wall
                                              ,spaceTexture = space
                                              ,targetTexture = target
                                              ,playerTexture = player
                                              ,bgTexture = bg}
                                  0)
     putStrLn (show (levels !! 0))
     --img <- loadBMP "./images/box.bmp"
     --display window background (drawing img)
     --display window background (render (levels !! 0))
     play window background fps (levels !! 0) render handleKeys update
     return ()
