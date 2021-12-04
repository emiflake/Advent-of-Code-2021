module Day04 (module Day04) where

import Common
import Control.Applicative hiding (many, some)
import Control.Arrow
import Data.Char
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Monoid
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Debug.Trace
import Parser
import Prelude

newtype Board = Board
  { getBoard :: Map (Int, Int) (Bool, Int)
  }
  deriving (Show, Eq)

data Input = Input
  { order :: [Int]
  , boards :: [Board]
  }
  deriving (Show, Eq)

pInput :: Parser Input
pInput =
  Input
    <$> pOrder
    <* sc
    <*> pBoards
  where
    ws = many " "
    int :: Parser Int
    int = (read <$> some digitChar) <* ws
    pOrder = int `sepBy` symbol ","
    pBoards = (toGameBoard . listsToMap <$> (some $ (ws >> some int) <* "\n")) `sepBy` "\n"

toGameBoard :: Map (Int, Int) Int -> Board
toGameBoard = Board . Map.map (False,)

checkOff :: Int -> Endo Board
checkOff p = Endo $ Board . Map.map (modIf ((== p) . snd) (first (const True))) . getBoard

projections :: Map (Int, Int) a -> [[a]]
projections b =
  let h = [[(x, y) | x <- [0 .. 4]] | y <- [0 .. 4]]
      v = transpose $ [[(i, i), (4 - i, i)] | i <- [0 .. 4]]
   in fmap (b Map.!) <$> concat [h, transpose h, v]

winning :: Board -> Maybe [Int]
winning board@(Board b) =
  fmap (const . fmap snd . filter (not . fst) $ Map.elems b) (find (all fst) $ projections b)

exe = putStrLn =<< either errorBundlePretty solve . runMyParser pInput <$> Text.readFile "inputs/4"
  where
    solve = show <<< p1 &&& p2

p1 = liftA2 play (.boards) (.order)
p2 = liftA2 play2 (.boards) (.order)

play bs (v : vs) =
  let new = appEndo (checkOff v) <$> bs
   in fromMaybe (play new vs) ((* v) . sum <$> firstJust winning new)

play2 [b] (v : vs) = play [b] (v : vs)
play2 bs (v : vs) =
  let new = appEndo (checkOff v) <$> bs
   in play2 (filter (isNothing . winning) new) vs
