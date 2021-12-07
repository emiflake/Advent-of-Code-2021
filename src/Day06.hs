module Day06 (module Day06) where

import Common
import Control.Arrow
import Data.Isomorphism
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Monoid
import Data.Semigroup
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Parser
import Prelude

-- | A naive approach, keep track of all the fish individually, and just iterate through them
naive :: Endo [Int]
naive = Endo $ \xs ->
  let (x, y) =
        unzip
          [ case x of
            0 -> (6, Just 8)
            _ -> (x - 1, Nothing)
          | x <- xs
          ]
   in x <> catMaybes y

pInput :: Parser [Int]
pInput = (read <$> some digitChar) `sepBy` ","

newtype School = School {getSchool :: Map Int Int}
  deriving stock (Show, Eq)

schoolIso :: Iso (->) [Int] School
schoolIso =
  Iso (School . frequencies) (concatMap (uncurry $ flip replicate) . Map.toList . getSchool)

endoSchool f = Endo $ \(School xs) -> School (f xs)

step :: Endo School
step = endoSchool $ \xs ->
  mconcat
    [ Map.mapKeysWith (+) (\case 0 -> 6; n -> n - 1) xs
    , Map.singleton 8 (fromMaybe 0 $ xs Map.!? 0)
    ]

schoolSize :: School -> Int
schoolSize = sum . fmap snd . Map.toList . getSchool

exe =
  either
    (putStrLn . errorBundlePretty)
    (print <<< solve 80 &&& solve 256)
    . runMyParser pInput
    =<< Text.readFile "inputs/6"
  where
    solve n =
      schoolSize . appEndo (stimes n step) . embed schoolIso
