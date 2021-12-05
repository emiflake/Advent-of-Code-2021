module Day05 (module Day05) where

import Common
import Control.Arrow
import Control.Monad
import Data.Bool
import Data.Char (chr, ord)
import Data.Foldable (traverse_)
import Data.List (maximumBy, minimumBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Ord (comparing)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Debug.Trace
import Linear.Compat ()
import Linear.V2
import Parser
import Prelude

data Rule = Rule {from :: V2 Int, to :: V2 Int}
  deriving (Show, Eq)

showMap :: Map (V2 Int) Int -> String
showMap map =
  let m = Map.toList map
      x0 = minimum $ fmap (._1.x) m
      x1 = maximum $ fmap (._1.x) m
      y0 = minimum $ fmap (._1.y) m
      y1 = maximum $ fmap (._1.y) m
   in unlines
        [ [ chr (ord '0' + (fromMaybe 0 $ map Map.!? (V2 x y)))
          | x <- [x0 .. x1]
          ]
        | y <- [y0 .. y1]
        ]

absRange x y = [min x y .. max x y]

renderSimp :: Rule -> Map (V2 Int) Int
renderSimp r
  | r.from.x == r.to.x = Map.fromList [(V2 r.from.x y, 1) | y <- absRange r.from.y r.to.y]
  | r.from.y == r.to.y = Map.fromList [(V2 x r.from.y, 1) | x <- absRange r.from.x r.to.x]
  | otherwise = Map.empty

renderVert :: Rule -> Map (V2 Int) Int
renderVert r
  | r.from.x == r.to.x = Map.fromList [(V2 r.from.x y, 1) | y <- absRange r.from.y r.to.y]
  | r.from.y == r.to.y = Map.fromList [(V2 x r.from.y, 1) | x <- absRange r.from.x r.to.x]
  | otherwise =
    let dx = r.to.x - r.from.x
        dy = r.to.y - r.from.y
     in Map.fromList . fmap ((,1) . uncurry V2) $ zip (bool reverse id (dx > 0) $ absRange r.from.x r.to.x) (bool reverse id (dy > 0) $absRange r.from.y r.to.y)

pInput =
  many . lexeme $ Rule <$> (V2 <$> int <* "," <*> int) <* " -> " <*> (V2 <$> int <* "," <*> int)
  where
    int = (read <$> some digitChar)

exe =
  either
    (putStrLn . errorBundlePretty)
    (void . runKleisli (solve renderSimp &&& solve renderVert))
    . runMyParser pInput
    =<< Text.readFile "inputs/5"
  where
    -- Do I really need Kleisli here? No, but I wanna be able to swap it out for `showMap` :)
    solve render =
      Kleisli $
        print
          . Map.size
          . Map.filter (>= 2)
          . foldl1 (Map.unionWith (+))
          . fmap render
