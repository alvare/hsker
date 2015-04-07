module Main where

import           Control.Applicative
import           Control.Monad.State.Strict
import           Control.Lens
import           Data.List.Lens
import           Graphics.Gloss.Interface.Pure.Game
import           System.Random

import           Types
import           Debug.Trace


initialGame = Game { _player = Player 0 0
                   , _enemies = []
                   , _rng = mkStdGen 10
                   , _timer = 0}

main :: IO ()
main = play (InWindow "Hskr" (640, 480) (30, 30))
            black
            60
            initialGame
            render
            handle
            update

render :: Game -> Picture
render g = Pictures $ (playerR $ g^.player) : (enemyR <$> g^.enemies)
  where
    playerR p = drawP `moved` (p^.x, p^.y)
    drawP = color white $ rectangleSolid 50 50

    enemyR e = drawE `moved` (e^.x, e^.y)
    drawE = color red $ rectangleSolid 50 50

    p `moved` (x', y') = translate x' y' p

handle :: Event -> Game -> Game
handle (EventMotion (dx, dy)) = execState $ do
    player .= Player dx dy
    me <- updateTimer
    case me of
        Just e -> enemies <>= [Enemy 100 100 E]
        Nothing -> return ()
  where
    updateTimer = do
        timer += 1
        timer' <- gets _timer
        case timer' > 100 of
            True -> do
                    timer .= 0
                    return . Just $ Enemy 0 0 E
            False -> return Nothing

handle _ = id

update :: Float -> Game -> Game
update dt = execState $ do
    enemies %= over mapped moveE
  where
    moveE e = case e^.direction of
                N -> e & y -~ dt * 50
                S -> e & y +~ dt * 50
                E -> e & x +~ dt * 50
                W -> e & x -~ dt * 50

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
