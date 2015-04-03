{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Types where

import Control.Lens
import System.Random

data Game = Game { _player :: Player
                 , _enemies :: [Enemy]
                 , _rng :: StdGen }

data Player = Player { _playerX :: Double
                     , _playerY :: Double }

data Direction = N | S | E | W
data Enemy = Enemy { _enemyX :: Double
                   , _enemyY :: Double
                   , _direction :: Direction }

$(makeLenses ''Game)
$(makeFields ''Player)
$(makeLenses ''Enemy)
$(makeFields ''Enemy)
