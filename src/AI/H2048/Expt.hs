module AI.H2048.Expt where

import           AI.H2048.Core
import           Control.Lens
import           Data.Function     (on)
import           Data.List         (maximum, maximumBy, sortBy, transpose)
import           Data.List.Ordered (isSorted)
import           Data.Maybe        (listToMaybe)
import           Data.Ord          (comparing)

data Prob = Prob Double Board

exptPlayer :: Player
exptPlayer = Player (PlayerName "Expt-AI") $ \b ->
  nextMove b

nextMove :: Board -> IO (Maybe Move)
nextMove b =
         return .
         listToMaybe .
         sortBy (flip compare `on` \m -> scored 2 [Prob 1 (move m b)] ) .
         filter (canMove b)
         $ options
  where
    scored :: Int -> [Prob] -> Double
    scored 0 ps = sum $ map s ps
      where
        s :: Prob -> Double
        s (Prob p b) = p * (fromInteger . toInteger . score $ b)
    scored n ps = scored (n-1) nexts
      where nexts = concatMap next ps
            next (Prob p b) = [Prob (p*0.1) (move m b')
                     | p * 0.1 > 0.0001,
                       b' <- map (nextBoard 4 b) (emptyPos b),
                       m <- filter (canMove b') options
                       ]
                  ++ [Prob (p*0.9) (move m b')
                     | p * 0.9 > 0.0001,
                       b' <- map (nextBoard 2 b) (emptyPos b),
                       m <- filter (canMove b') options
                       ]


avg l | len == 0 = 0
      | otherwise = sum l `div` len
      where len = length l

nextBoard :: Int -> Board -> (Int,Int) -> Board
nextBoard v b (x,y) = b {_cells= (_cells b) & ix x . ix y ^~ v}

score :: Board -> Int
score b = 100000 + orderScore + closePairsScore  + maxAtEndScore + emptyScore - divergeScore -- - total * 10 + emptyScore
  where
    emptyScore = 4000 * en
    total = sum (map sum (_cells b))
    stat = toStat b
    en = emptyNum stat
    log2Int :: Int -> Int
    log2Int = floor . (5^) . floor .  logBase 2.0 . fromIntegral
    orderScore = 8 * sum (map log2Int (ordered stat))
    maxAtEndScore = 5 * sum (map log2Int (maxAtEnd stat))
    closePairsScore = 2 * sum (map log2Int cp) -  400 * length cp
    divergeScore = 10 * sum (map (8^) dv) + 800 * length dv
    cp = closePairs stat
    dv = diverge stat
