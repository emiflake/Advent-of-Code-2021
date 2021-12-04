module Day03 (module Day03) where

import Common
import Control.Applicative
import Control.Arrow
import Data.Char
import Data.List
import Debug.Trace
import Parser
import Prelude

exe = (p1 &&& p2) . lines <$> readFile "inputs/3"

bin2dec xs = sum [(ord d - ord '0') * (2 ^ m) | (m, d) <- zip [0 ..] (reverse xs)]

mostCommon c = head . head . sortOn (both c <<< length &&& (ord . head)) . group . sort

rate c = bin2dec . fmap (mostCommon c) . transpose

rate2 c [n] = n
rate2 c ns = (:) <*> (rate2 c . fmap tail . flip filter ns . (. head) . (==)) $ mostCommon c (head <$> ns)

p1 = liftA2 (*) (rate negate) (rate id)

p2 = apOn (*) bin2dec (rate2 negate) (rate2 id)
