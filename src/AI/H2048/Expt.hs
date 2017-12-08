module AI.H2048.Expt where

import           AI.H2048.Core
import           Control.Lens
import           Data.List           (sortBy, maximum, maximumBy, transpose)
import           Data.Ord            (comparing)
import Data.Maybe (listToMaybe)
import Data.Function (on)

exptPlayer :: Player
exptPlayer = Player (PlayerName "Expt-AI") $ \b ->
  return . listToMaybe $ findBestMove b

-- Find best move
findBestMove :: Board -> [Move]
findBestMove b =  sortBy (flip compare `on` \m-> scoreTopLevel b m).
  filter (canMove b) $ options

scoreTopLevel :: Board -> Move -> Double
scoreTopLevel b m
    | movedBoard == b   = 0.0
    | otherwise         = expectiMiniMax 2 . RandNode 1.0 $ movedBoard
        where movedBoard = move m b

-- Expectiminimax algorithm
type Prob = Double

data GameTree = PlayNode Prob Board | RandNode Prob Board
    deriving (Show, Eq)

getBoard :: GameTree -> Board
getBoard (PlayNode _ b) = b
getBoard (RandNode _ b) = b

getProb :: GameTree -> Prob
getProb (PlayNode p _) = p
getProb (RandNode p _) = p

getChildren :: GameTree -> [GameTree]
getChildren (PlayNode p b)  = map (RandNode p) . filter (/=b) $ move <$> options <*> pure b
getChildren (RandNode p b)  = concat . map placeTile $ [(2, 0.75), (4, 0.25)]
    where placeTile (x, c)  = map (PlayNode ((p / numEmpties (b)) * c)) . fillEmpties b x . empties $ b
          numEmpties        = fromIntegral . length . empties
          empties           = emptyPos

fillEmpties :: Board -> Int -> [(Int, Int)] -> [Board]
fillEmpties b@Board{_cells=cells} v = map $ \(x,y) ->
  b{_cells=cells & ix x . ix y .~ v}

expectiMiniMax :: Int -> GameTree -> Double
expectiMiniMax x tree
    | getChildren (tree) == []          = 0.0
    | getProb tree < 0.0001 || x == 0   = scoreBoard . getBoard $ tree

expectiMiniMax d n@(PlayNode _ _)   = maximum . map (expectiMiniMax (d-1)) . getChildren $ n
expectiMiniMax d n@(RandNode _ _)   = sum . map (weightScore) . getChildren $ n
    where weightScore n@(PlayNode p _) = p * expectiMiniMax (d-1) n

-- Scoring algorithm
scoreBoard :: Board -> Double
scoreBoard b@Board{_cells = cells} = fromIntegral $ scoreRows cells + scoreCols cells + 100000 - boolToInt (isFinish b) * 1000000
    where scoreRows = sum . map scoreRow
          scoreCols = scoreRows . transpose

isFinish :: Board -> Bool
isFinish b = null . map (canMove b) $ options

boolToInt b   =   if b then 1 else 0

scoreRow :: [Int] -> Int
scoreRow r = sum . zipWith (*) weights $ heuristics r
    where
          weights       =   [10000, 1000, 20000, 10000, 10000]
          heuristics r  =   [ countOfElem 0 r
                            , closePairs r
                            , boolToInt . maxAtEnd $ r
                            , boolToInt . isOrdered (<) $ r
                            , boolToInt . isOrdered (>) $ r
                            ]
-- Heuristic algorithms
countOfElem :: Eq a => a -> [a] -> Int
countOfElem elem = length . filter (==elem)

isOrdered :: (Int -> Int -> Bool) -> [Int] -> Bool
isOrdered f = and . pairwise f

closePairs :: [Int] -> Int
closePairs = countOfElem True . pairwise close
    where close x y
            | x == 0 || y == 0  = False
            | otherwise         = max x y - min x y == min x y

maxAtEnd :: [Int] -> Bool
maxAtEnd xs
    | maxIndex xs == 0 || maxIndex xs == 3  = True
    | otherwise                             = False
        where maxIndex xs = snd . maximumBy (comparing fst) $ zip xs [0..]

pairwise :: (Int -> Int -> Bool) -> [Int] -> [Bool]
pairwise f r@(x:xs) = zipWith f r xs
