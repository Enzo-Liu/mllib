module AI.H2048.Expt where

import AI.H2048.Core

exptPlayer :: Player
exptPlayer = Player (PlayerName "Expt-AI") $
  \b-> return . Just $ DOWN
