module H2048 where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Loops (iterateUntilM)
import           Data.List           (intercalate)
import           Data.List.NonEmpty  (NonEmpty (..))
import qualified Data.List.NonEmpty  as NE
import           Data.Maybe
import           System.Random       (randomRIO)



newtype ColSize = ColSize Int deriving Show
newtype RowSize = RowSize Int deriving Show

data Board = Board
  { _colSize :: ColSize
  , _rowSize :: RowSize
  , _cells   :: [[Int]]
  , _pos     :: [[(Int, Int)]]
  }
instance Show Board where
  show Board{_cells=cells} = unlines $ fmap format cells
    where format = unwords . fmap formatNum
          formatNum n = let ns = show n
                            l = length ns
                            toFill | l > 4 = l
                                   | otherwise = l - 4
                        in ns ++ replicate toFill ' '
instance Eq Board where
  Board{_cells=c1} == Board{_cells=c2} = c1 == c2

data Move = UP | DOWN | LEFT | RIGHT deriving (Show, Eq)

makeBoard :: ColSize -> RowSize -> Board
makeBoard c@(ColSize col) r@(RowSize row) = Board c r board' pos'
  where
    board' = replicate row . replicate col $ 0
    pos' = fmap (\i -> fmap ((,)i) [0..col-1]) [0..row-1]

pickRandom :: NonEmpty a -> IO a
pickRandom xs = (xs NE.!!) <$> randomRIO (0, NE.length xs - 1)

pickRandom' :: [a] -> IO (Maybe a)
pickRandom' xs = (\i -> xs ^? ix i) <$> randomRIO (0, length xs - 1)

randomValue :: IO Int
randomValue = pickRandom $ NE.fromList [2, 2, 2, 4]

emptyPos :: Board -> [(Int, Int)]
emptyPos Board {_cells = cells, _pos = pos} =
  concat $ map fst . filter (\(_, v) -> v == 0) <$> liftA2 zip pos cells

addRandom :: Board -> IO (Maybe Board)
addRandom b@Board {_cells = cells} = do
  v <- randomValue
  fmap (fmap (\(c,r)-> b {_cells = cells & ix c . ix r .~ v})) (randomPos b)
  where
    randomPos = pickRandom' . emptyPos

squeeze :: [Int] -> [Int]
squeeze line = valued ++ replicate toFill 0
  where
    l = length line
    valued = filter (/= 0) line
    toFill = l - length valued

merge :: [Int] -> [Int]
merge [] = []
merge [a] = [a]
merge (a:b:xs)
  | a == b = a + b : 0 : merge xs
  | otherwise = a : merge (b : xs)

move :: Move -> Board -> Board
move LEFT b@Board{_cells=cells} = b {_cells = fmap (squeeze . merge) cells}
move UP Board{_cells=cells}     = undefined
move DOWN b                     = undefined
move RIGHT b                    = undefined

main = let b = makeBoard (ColSize 4) (RowSize 4) in
  flip (iterateUntilM isNothing) (Just b) $ \b' -> do
    let b'' = move LEFT . fromJust $ b'
    print b''
    jb <- addRandom  b''
    return jb
