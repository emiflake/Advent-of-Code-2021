module Common (module Common) where

import Control.Applicative
import Control.Arrow
import Data.Function
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Monoid (Endo (Endo), appEndo)
import Data.Semigroup (stimes)

zipOn :: forall f a b. Applicative f => (a -> b) -> f a -> f (a, b)
zipOn f xs = ((,) <*> f) <$> xs

both :: forall a b. (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

apOn :: forall f a b c. Applicative f => (b -> b -> c) -> (a -> b) -> f a -> f a -> f c
apOn (<>) f a b = (<>) <$> (f <$> a) <*> (f <$> b)

modIf :: forall a. (a -> Bool) -> (a -> a) -> (a -> a)
modIf p f x
  | p x = f x
  | otherwise = x

filterEndo :: forall a. (a -> Bool) -> Endo a -> Endo a
filterEndo p = Endo . modIf p . appEndo

firstJust :: forall a b. (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . catMaybes . fmap f

-- I've written this enough times by now, lol.
listsToMap :: forall a e. (Num e, Enum e, Ord e) => [[a]] -> Map (e, e) a
listsToMap lists =
  Map.fromList
    [ ((x, y), v)
    | (y, list) <- zip [0 ..] lists
    , (x, v) <- zip [0 ..] list
    ]

-- | enumFromTo that works both ways
dynamicRange :: (Ord a, Enum a) => a -> a -> [a]
dynamicRange x y
  | x > y = reverse [y .. x]
  | otherwise = [x .. y]

nTimes :: Int -> (a -> a) -> a -> a
nTimes n = appEndo . stimes n . Endo

invFrequencies :: Ord a => [a] -> Map Int [a]
invFrequencies =
  foldl1 (Map.unionWith (<>))
    . fmap (uncurry Map.singleton <<< length &&& (: []) . head)
    . group
    . sort

frequencies :: Ord a => [a] -> Map a Int
frequencies = Map.fromList . fmap (head &&& length) . group . sort
