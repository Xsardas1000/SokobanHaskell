module Lib (
   Menu(..)
  ,Game(..)
  ,SokobanMap(..)
  ,MapTextures(..)
  ,Coord(..)
) where

import Graphics.Gloss.Data.Picture
import Data.Maybe

type Coord = (Int, Int)

data Game = Game
                {sokobanMaps :: [SokobanMap]
                ,savedMaps   :: [SokobanMap]
                ,currMap     :: SokobanMap
                ,backupMap   :: SokobanMap
                ,currNumber  :: Int
                ,steps		   :: Int
                ,menu        :: Menu
                ,savesMenu   :: SavesMenu
                ,state       :: Int   -- 0 if currMap, 1 if Menu
                ,scaleAll    :: Float
				        ,labelSteps  :: Picture
                ,textures    :: MapTextures
                } deriving Show

data Menu = Menu
                {menuBackground   :: Picture
                ,labelHeader      :: Picture
                ,labelScaleInc    :: Picture
                ,labelScaleDec    :: Picture
                ,labelRestart     :: Picture
                ,labelRestartGame :: Picture
                ,labelPrevState   :: Picture
                } deriving Show

data SavesMenu
          {savesBackground  :: Picture
          ,labelHeader      :: Picture
          ,savesList        :: Picture
          ,numSaves         :: Picture
          }

data SokobanMap = SokobanMap
                          {size      :: Int
                          ,mapNumber :: Int
                          ,sizeX     :: Int
                          ,sizeY     :: Int
                          ,walls     :: [Coord]
                          ,targets   :: [Coord]
                          ,boxes     :: [Coord]
                          ,darkboxes :: [Coord]
                          ,spaces    :: [Coord]
                          ,player    :: Coord
                          ,previousMap :: Maybe SokobanMap
                          } deriving Show

data MapTextures = MapTextures
                    {bgTexture      :: Picture
                    ,wallTexture    :: Picture
                    ,spaceTexture   :: Picture
                    ,boxTexture     :: Picture
                    ,darkboxTexture :: Picture
                    ,targetTexture  :: Picture
                    ,playerTexture  :: Picture
                    } deriving Show
