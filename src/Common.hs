{-# LANGUAGE RankNTypes #-}

module Common (module Common) where

import Control.Applicative
import Control.Arrow
import Control.Monad.IO.Class
import Control.Natural
import Data.Function
import Data.Functor
import Data.Functor.Compose
import Data.Functor.Contravariant
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Monoid (Endo (Endo), appEndo)
import Data.Semigroup (stimes)
import Data.Text.IO qualified as Text
import Parser

--------------------------------------------------------------------------------
-- AOC specific

solveOnParse :: forall m i o. (Monad m, Show o) => Parser i -> (i -> m o) -> (i -> m o) -> (m ~> IO) -> FilePath -> IO (Maybe (o, o))
solveOnParse parser p1 p2 trans fp =
  either
    (fmap (const Nothing) . putStrLn . errorBundlePretty)
    (getCompose . fmap Just . Compose . runKleisli $ knat trans (Kleisli p1 &&& Kleisli p2))
    . runMyParser parser
    =<< (Text.readFile fp)

knat :: forall f g a b. (f ~> g) -> Kleisli f a b -> Kleisli g a b
knat nt = Kleisli . (nt .) . runKleisli

--------------------------------------------------------------------------------
-- General

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
dynamicRange x y = [min x y .. max x y]

nTimes :: Int -> (a -> a) -> a -> a
nTimes n = appEndo . stimes n . Endo

invFrequencies :: Ord a => [a] -> Map Int [a]
invFrequencies =
  foldl1 (Map.unionWith (<>))
    . fmap (uncurry Map.singleton <<< length &&& (: []) . head)
    . group
    . sort

-- | Create a map from element to how often it appears
frequencies :: Ord a => [a] -> Map a Int
frequencies = Map.fromList . fmap (head &&& length) . group . sort

-- | Get the element in the middle of the list
middle :: Ord a => [a] -> a
middle xs = (xs !!) . (`div` 2) $ length xs

--------------------------------------------------------------------------------
-- Numeric

fmean :: (Fractional a, Num a) => [a] -> a
fmean = liftA2 (/) sum (fromInteger . fromIntegral . length)

imean :: (Integral a, Num a) => [a] -> a
imean = liftA2 div sum (fromInteger . fromIntegral . length)

softMedian :: (Ord a, Enum a, Num a) => [a] -> a
softMedian = middle . sort

(+/-) :: (Ord a, Num a, Enum a) => a -> a -> [a]
(+/-) x pm = dynamicRange (x - pm) (x + pm)
