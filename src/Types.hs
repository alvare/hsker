{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Types where

import FRP.Helm.Time (Time)
import Control.Lens
import System.Random

data Game = Game { _player :: Player
                 , _enemies :: [Enemy]
                 , _rng :: StdGen }
    deriving Show

data Player = Player { _playerX :: Double
                     , _playerY :: Double }
    deriving Show

data Direction = N | S | E | W deriving Show
data Enemy = Enemy { _enemyX :: Double
                   , _enemyY :: Double
                   , _direction :: Direction }
    deriving Show

$(makeLenses ''Game)
$(makeFields ''Player)
$(makeLenses ''Enemy)
$(makeFields ''Enemy)
