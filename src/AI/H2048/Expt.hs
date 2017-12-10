module AI.H2048.Expt where

import           AI.H2048.Core
import           Control.Lens
import           Data.Function     (on)
import           Data.List         (maximum, maximumBy, sortBy, transpose)
import           Data.List.Ordered (isSorted)
import           Data.Maybe        (listToMaybe)
import           Data.Ord          (comparing)

exptPlayer :: Player
exptPlayer = Player (PlayerName "Expt-AI") $ \b ->
  undefined
