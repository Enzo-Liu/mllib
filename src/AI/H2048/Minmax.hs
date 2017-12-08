module AI.H2048.Minmax(minmaxPlayer) where

import           AI.H2048.Core
import           Data.Function     (on)
import           Data.List         (sortBy, transpose)
import           Data.List.Ordered (isSorted)
import           Data.Maybe        (listToMaybe)


minmaxPlayer :: Player
minmaxPlayer =
    Player (PlayerName "Minmax-AI") nextMove

nextMove :: Board -> IO (Maybe Move)
nextMove b =
         return .
         listToMaybe .
         sortBy (flip compare `on` \m -> scored 3 m b) .
         filter (canMove b)
         $ options
  where
    scored :: Int -> Move -> Board -> Int
    scored 0 _ _  = 0
    scored 1 m b' = score $ move m b'
    scored n m b' = sum . map (\m' -> scored (n-1) m' (move m b')) $ options

score :: Board -> Int
score b@Board{_cells = cells} =
    (1000 `div` (empty+1)) +
    -- value +
    sortScore - penalty
  where
    empty = emptyCellNum b -- 0-16
    value :: Int
    value = sum $ map (sum . map ((^ 3) . log2Int)) cells -- 1 ~ 200
    log2Int :: Int -> Int
    log2Int = floor . logBase 2.0 . fromIntegral
    sortScore =
        maximum .
        map
            (\(t1,t2) ->
                  sum (map lineSortScore $ t1 cells) +
                  sum (map lineSortScore $ t2 cells)) $
        [ (id, rotate270)
        , (id, rotate90)
        , (rotate180, rotate90)
        , (rotate90, rotate180)
        , (rotate270, rotate180)
        , (rotate180, rotate270)]
    lineSortScore l =
        if isSorted l
            then sum $ map ((\t->t^t) . log2Int) l
            else 0
    rotate270 = transpose . reverse
    rotate90 = reverse . transpose
    rotate180 = reverse . transpose
    penalty = minimum . map divergeScore $ [id, rotate180, rotate90, rotate270]
    divergeScore t = sum . map divergeLineScore $ t cells
    divergeLineScore :: [Int] -> Int
    divergeLineScore [] = 0
    divergeLineScore [_] = 0
    divergeLineScore (a:b':xs) = 4^(abs (log2Int a - log2Int b')) + divergeLineScore (b':xs)
