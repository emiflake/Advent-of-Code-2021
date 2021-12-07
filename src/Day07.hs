{-# LANGUAGE BlockArguments #-}

module Day07 (module Day07) where

import Common
import Control.Applicative hiding (some)
import Control.Arrow
import Control.Lens.Combinators (alaf)
import Data.Composition ((.:))
import Data.Functor.Identity
import Data.Isomorphism
import Data.List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Monoid
import Data.Ord (comparing)
import Data.Semigroup
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Parser
import Prelude

pInput :: (Read a, Num a) => Parser [a]
pInput = (read <$> some digitChar) `sepBy` ","

absDiff = abs .: (-)

tri n = (n * succ n) `div` 2

triDiff = tri .: absDiff

exe =
  solveOnParse @Identity
    (pInput @Int)
    (solve absDiff $ liftA2 dynamicRange imean softMedian)
    (solve triDiff $ (+/- 1) . imean)
    (pure . runIdentity)
    "inputs/7"
  where
    solve diff gen is =
      pure $ minimum do n <- gen is; pure (alaf Sum foldMap (diff n) is)
