module AI.H2048.Minmax(minmaxPlayer) where

import AI.H2048.Core

minmaxPlayer :: Player
minmaxPlayer = Player (PlayerName "Minmax-AI") $
  -- \b-> zipWith (\m-> (m, move m b)) options
  undefined

score :: Board -> Int
score b@Board{_cells=cells} = empty * 10
  where empty = emptyCellNum b
