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

renderSimp :: Rule -> Map (V2 Int) Int
renderSimp r
  | r.from.x == r.to.x = Map.fromList [(V2 r.from.x y, 1) | y <- dynamicRange r.from.y r.to.y]
  | r.from.y == r.to.y = Map.fromList [(V2 x r.from.y, 1) | x <- dynamicRange r.from.x r.to.x]
  | otherwise = Map.empty

renderVert :: Rule -> Map (V2 Int) Int
renderVert r
  | r.from.x == r.to.x || r.from.y == r.to.y = renderSimp r
  | otherwise =
    Map.fromList
      [ (V2 x y, 1)
      | (x, y) <- zip (dynamicRange r.from.x r.to.x) (dynamicRange r.from.y r.to.y)
      ]

pInput =
  many . lexeme $ Rule <$> (V2 <$> int <* "," <*> int) <* " -> " <*> (V2 <$> int <* "," <*> int)
  where
    int = (read <$> some digitChar)

-- Do I really need Kleisli here? No, but I wanna be able to swap it out for `showMap` :)
exe =
  either
    (putStrLn . errorBundlePretty)
    (void . runKleisli (Kleisli (solve renderSimp) &&& Kleisli (solve renderVert)))
    . runMyParser pInput
    =<< Text.readFile "inputs/5"
  where
    solve render =
      print
        . Map.size
        . Map.filter (>= 2)
        . foldl1 (Map.unionWith (+))
        . fmap render
