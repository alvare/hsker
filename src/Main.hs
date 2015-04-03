{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Main where

import           Control.Applicative
import           Control.Monad.State.Strict
import           Control.Lens
import           FRP.Helm
import           FRP.Helm.Time
import qualified FRP.Helm.Mouse as M
import qualified FRP.Helm.Window as W
import           System.Random

import           Types
import           Debug.Trace


main :: IO ()
main = run defaultConfig $ render <$> W.dimensions <*> stepper
  where
    stepper = foldp step game $ (,) <$> M.position <*> every second
    game = Game { _player = Player 0 0
                , _enemies = []
                , _rng = mkStdGen 10 }

render :: (Int, Int) -> Game -> Element
render (w, h) game = collage w h $ playerR : enemiesR
  where
    player' = game ^. player
    enemies' = game ^. enemies
    playerR = move (player'^.x, player'^.y) . filled white $ square 40
    enemiesR = map (\e -> move (e^.x, e^.y) . filled red $ square 40) enemies'

step :: ((Int, Int), Time) -> Game -> Game
step ((dx, dy), time) = execState $ do
    player . x .= realToFrac dx
    player . y .= realToFrac dy
    spaWn
    enemies %= over mapped moveE
  where
    moveE :: Enemy -> Enemy
    moveE e = case e^.direction of
                N -> e & y -~ 1
                S -> e & y +~ 1
                E -> e & x +~ 1
                W -> e & x -~ 1
spaWn = do
    g <- gets _rng
    let (v, g') = randomR (0, 100) g :: (Int, StdGen)
    rng .= g'
    case v of
        50 -> enemies %= (Enemy 100 100 N :)
        _ -> return ()
