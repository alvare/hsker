{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Types where

import Control.Lens
import System.Random

data Game = Game { _player :: Player
                 , _enemies :: [Enemy]
                 , _rng :: StdGen
                 , _timer :: Int }
    deriving Show

data Player = Player { _playerX :: Float
                     , _playerY :: Float }
    deriving Show

data Direction = N | S | E | W deriving Show
data Enemy = Enemy { _enemyX :: Float
                   , _enemyY :: Float
                   , _direction :: Direction }
    deriving Show

$(makeLenses ''Game)
$(makeLenses ''Player)
$(makeLenses ''Enemy)

$(makeFields ''Player)
$(makeFields ''Enemy)
