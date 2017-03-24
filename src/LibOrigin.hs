module Lib (
   Menu(..)
  ,Game(..)
  ,SokobanMap(..)
  ,MapTextures(..)
) where

import Graphics.Gloss.Data.Picture

type Coord = (Int, Int)

data Game = Game
                {sokobanMaps :: [SokobanMap]
                ,currMap     :: SokobanMap
                ,backupMap   :: SokobanMap
                ,stateBackup :: SokobanMap
                ,currNumber  :: Int
                ,menu        :: Menu
                ,state       :: Int   -- 0 if currMap, 1 if Menu
                ,scaleAll    :: Float
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

data MapTextures = MapTextures
                    {bgTexture      :: Picture
                    ,wallTexture    :: Picture
                    ,spaceTexture   :: Picture
                    ,boxTexture     :: Picture
                    ,darkboxTexture :: Picture
                    ,targetTexture  :: Picture
                    ,playerTexture  :: Picture
                    } deriving Show
