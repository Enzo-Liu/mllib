module H2048AI
  ( minmaxPlayer
  , randomPlayer
  , exptPlayer
  , Player(..)
  , PlayerName(..)
  , playerName
  , getMove
  ) where

import qualified Data.List.NonEmpty as NE
import           H2048

newtype PlayerName = PlayerName String deriving Eq
data Player = Player PlayerName (Board -> IO (Maybe Move))

instance Eq Player where
  (Player n1 _) == (Player n2 _) = n1 == n2

playerName :: Player -> String
playerName (Player (PlayerName n) _) = n

minmaxPlayer :: Player
minmaxPlayer = Player (PlayerName "Minmax-AI") $
  \b-> return . Just $ UP

exptPlayer :: Player
exptPlayer = Player (PlayerName "Expt-AI") $
  \b-> return . Just $ DOWN

options :: [Move]
options = [UP, DOWN, LEFT, RIGHT]

randomPlayer :: Player
randomPlayer = Player (PlayerName "Random-AI") $
  \b -> pickRandom' $ filter (canMove b) options

canMove :: Board -> Move -> Bool
canMove b m = move m b /= b

getMove :: Player -> Board -> IO (Maybe Move)
getMove (Player _ strategy) = strategy
