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
main = run defaultConfig $ render <$> W.dimensions <*> gameSignal

gameSignal :: Signal Game
gameSignal = foldp step initialGame $ (,) <$> enemiesSignal <*> playerSignal
  where
    initialGame = Game { _player = Player 0 0
                       , _enemies = []
                       , _rng = mkStdGen 10 }
    step :: ([Enemy], Player) -> Game -> Game
    step (es, p) game = game { _player = p
                             , _enemies = es }

enemiesSignal :: Signal [Enemy]
enemiesSignal = foldp step [Enemy 100 100 E] (fps 30)
  where
    step dt = map (moveE dt)
    moveE dt e = case e^.direction of
                N -> e & y -~ dt * 0.4
                S -> e & y +~ dt * 0.4
                E -> e & x +~ dt * 0.4
                W -> e & x -~ dt * 0.4

playerSignal :: Signal Player
playerSignal = foldp step (Player 0 0) $ M.position
  where
    step :: (Int, Int) -> Player -> Player
    step (mx, my) _ = Player (realToFrac mx) (realToFrac my)

--spaWn = do
--    g <- gets _rng
--    let (v, g') = randomR (0, 10) g :: (Int, StdGen)
--    rng .= (traceShow v $ g')
--    case v of
--        1 -> enemies %= (Enemy 200 200 N :)
--        2 -> enemies %= (Enemy 200 200 S :)
--        3 -> enemies %= (Enemy 200 200 E :)
--        4 -> enemies %= (Enemy 200 200 W :)
--        _ -> return ()

render :: (Int, Int) -> Game -> Element
render (w, h) game = collage w h $ playerR : enemiesR
  where
    player' = game ^. player
    enemies' = game ^. enemies
    playerR = move (player'^.x, player'^.y) . filled white $ square 40
    enemiesR = map (\e -> move (e^.x, e^.y) . filled red $ square 40) enemies'
