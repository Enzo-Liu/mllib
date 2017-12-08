{-# LANGUAGE OverloadedStrings #-}

module H2048UI where

import           Brick
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import           Control.Monad              (void)
import           Control.Monad.IO.Class
import           Data.Maybe
import qualified Graphics.Vty               as V
import           H2048

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

app :: App Board () Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

continueMove :: Move -> Board -> EventM Name (Next Board)
continueMove m b = do
  let b' = move m b
  if b' == b
    then continue b
    else liftIO (addRandom b') >>= (continue . fromJust)

-- Handling events
handleEvent :: Board -> BrickEvent Name () -> EventM Name (Next Board)
handleEvent b (VtyEvent (V.EvKey V.KUp []))         = continueMove UP b
handleEvent b (VtyEvent (V.EvKey V.KDown []))       = continueMove DOWN b
handleEvent b (VtyEvent (V.EvKey V.KRight []))      = continueMove RIGHT b
handleEvent b (VtyEvent (V.EvKey V.KLeft []))       = continueMove LEFT b
handleEvent b (VtyEvent (V.EvKey (V.KChar 'k') [])) = continueMove UP b
handleEvent b (VtyEvent (V.EvKey (V.KChar 'j') [])) = continueMove DOWN b
handleEvent b (VtyEvent (V.EvKey (V.KChar 'l') [])) = continueMove RIGHT b
handleEvent b (VtyEvent (V.EvKey (V.KChar 'h') [])) = continueMove LEFT b
handleEvent _ (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO initBoard >>= continue
handleEvent b (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt b
handleEvent b (VtyEvent (V.EvKey V.KEsc []))        = halt b
handleEvent b _                                     = continue b

-- Drawing

newtype Cell = Cell Int

drawUI :: Board -> [Widget Name]
drawUI b = [drawCells b]

drawCells :: Board -> Widget Name
drawCells b = C.center $ withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "2048")
  $ vBox rows
  where
    (ColSize c)  = _colSize b
    (RowSize r)  = _rowSize b
    cells :: [[Int]]
    cells  = _cells b
    rows         = [hBox $ cellsInRow y | y <- [0..r-1]]
    cellsInRow :: Int -> [Widget Name]
    cellsInRow y = [drawCoord (Cell $ cells !! y !! x) | x <- [0..c-1]]
    drawCoord    = drawCell


drawCell :: Cell -> Widget Name
drawCell (Cell n) = withAttr (attr n) $ cw n

cw :: Int -> Widget Name
cw 0 = str "    "
cw i = str $ formatNum i
  where
    formatNum n =
      let ns = show n
          l = length ns
          toFill
            | l < 4 = 4 - l
            | otherwise = l
      in ns ++ replicate toFill ' '

attr :: Int -> AttrName
attr = attrName . ("cell" ++) . show

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ ("cell0", rgbfg 238 228 218)
    , ("cell2", rgbfg 238 228 218)
    , ("cell4", rgbfg 237 224 200)
    , ("cell8", rgbfg 242 177 121)
    , ("cell16", rgbfg 245 149 99)
    , ("cell32", rgbfg 246 124 95)
    , ("cell64", rgbfg 246 94 59)
    , ("cell128", rgbfg 237 207 114)
    , ("cell256", rgbfg 237 204 97)
    , ("cell512", rgbfg 237 200 80)
    , ("cell1024", rgbfg 237 197 63)
    , ("cell2048", rgbfg 237 194 46)
    ]
  where
    rgbfg :: Integer -> Integer -> Integer -> V.Attr
    rgbfg r g b= fg $ V.rgbColor r g b

getMove :: IO Move
getMove = fmap read getLine

initBoard :: IO Board
initBoard = fmap fromJust $ addRandom $ makeBoard (ColSize 4) (RowSize 4)

h2048UI :: IO ()
h2048UI = do
  b <- initBoard
  void $ customMain (V.mkVty V.defaultConfig) Nothing app b
