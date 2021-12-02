module Day02 (module Day02) where

import Control.Applicative (liftA2)
import Control.Arrow ((&&&), (<<<))
import Data.Foldable (asum)
import Data.Functor ((<&>))
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Parser
import Prelude

data Instruction
  = Forward Integer
  | Down Integer
  | Up Integer
  deriving (Show)

exe = fmap p12 . runMyParser pInstructions <$> Text.readFile "inputs/2"

pInstructions :: Parser [Instruction]
pInstructions =
  some . lexeme $
    asum
      [ lexeme "forward" *> int <&> Forward
      , lexeme "down" *> int <&> Down
      , lexeme "up" *> int <&> Up
      ]
  where
    int :: Parser Integer
    int = read <$> some digitChar

data Sub = Sub {horizontal :: Integer, depth :: Integer, aim :: Integer} deriving (Show)

p12 :: [Instruction] -> (Integer, Integer)
p12 =
  liftA2 (*) (.horizontal) (.aim)
    &&& liftA2 (*) (.horizontal) (.depth)
    <<< foldl f (Sub 0 0 0)
  where
    f :: Sub -> Instruction -> Sub
    f s (Down n) = s {aim = s.aim + n}
    f s (Up n) = s {aim = s.aim - n}
    f s (Forward n) = s {horizontal = s.horizontal + n, depth = s.depth + s.aim * n}

tutorialInstructions :: [Instruction]
tutorialInstructions =
  [ Forward 5
  , Down 5
  , Forward 8
  , Up 3
  , Down 8
  , Forward 2
  ]

-- instructions :: [Instruction]
-- instructions = [Forward 6, Forward 6, Down 6, Down 5, Up 2, Forward 4, Forward 8, Up 9, Forward 3, Down 1, Forward 2, Forward 3, Down 1, Down 3, Down 3, Forward 9, Down 6, Forward 1, Up 5, Down 1, Forward 8, Forward 7, Up 8, Forward 7, Down 9, Forward 5, Forward 4, Forward 2, Down 3, Down 6, Down 6, Down 5, Forward 5, Forward 4, Forward 8, Up 3, Forward 6, Down 4, Up 2, Forward 8, Forward 6, Forward 1, Up 2, Down 9, Down 9, Up 5, Down 1, Up 1, Up 3, Forward 2, Forward 4, Down 8, Down 1, Up 1, Up 4, Up 1, Up 2, Forward 4, Down 1, Forward 1, Down 3, Forward 4, Down 1, Down 6, Forward 3, Down 9, Down 4, Up 4, Down 3, Forward 4, Down 3, Up 4, Forward 5, Down 9, Forward 4, Forward 1, Forward 1, Forward 4, Up 6, Up 9, Down 1, Down 1, Forward 6, Down 1, Down 5, Down 4, Forward 8, Down 8, Down 2, Down 5, Down 6, Down 4, Down 9, Up 8, Down 4, Forward 5, Up 6, Forward 2, Forward 9, Down 5, Forward 3, Forward 6, Down 9, Up 3, Forward 7, Forward 1, Forward 1, Up 6, Forward 3, Down 3, Down 1, Up 7, Forward 2, Forward 9, Forward 4, Down 9, Forward 4, Forward 5, Up 7, Down 1, Up 9, Down 6, Up 5, Forward 9, Forward 9, Down 4, Forward 1, Forward 2, Forward 1, Down 2, Forward 7, Up 6, Up 5, Up 6, Down 4, Down 6, Down 9, Forward 9, Down 9, Down 1, Down 2, Up 7, Forward 3, Down 2, Up 8, Forward 5, Forward 2, Up 2, Down 9, Down 2, Down 8, Forward 8, Down 2, Down 8, Forward 3, Up 1, Forward 7, Forward 1, Up 9, Forward 1, Forward 1, Forward 1, Down 1, Down 6, Forward 2, Down 8, Down 9, Forward 3, Up 9, Down 5, Down 2, Forward 7, Forward 1, Forward 6, Down 5, Down 4, Down 2, Down 7, Down 1, Forward 8, Down 3, Up 8, Forward 2, Down 6, Forward 9, Up 6, Forward 3, Forward 7, Down 3, Down 8, Down 8, Down 7, Down 8, Forward 3, Down 1, Forward 4, Down 8, Forward 1, Forward 1, Forward 4, Forward 6, Up 9, Forward 8, Up 6, Forward 4, Forward 4, Down 1, Down 7, Up 9, Forward 5, Down 9, Down 1, Up 2, Down 7, Forward 8, Forward 9, Forward 6, Forward 8, Up 1, Forward 2, Down 7, Up 9, Up 5, Forward 6, Forward 7, Down 4, Forward 1, Down 2, Down 7, Down 4, Down 8, Down 4, Forward 7, Down 2, Down 7, Forward 5, Down 3, Forward 6, Up 5, Up 9, Down 5, Up 2, Up 6, Forward 6, Forward 9, Down 8, Forward 8, Forward 8, Forward 3, Up 2, Forward 4, Down 9, Down 3, Up 2, Down 9, Forward 9, Forward 8, Forward 6, Forward 4, Up 8, Down 3, Up 7, Up 7, Up 5, Up 3, Forward 3, Up 7, Up 8, Down 6, Down 3, Down 4, Down 1, Forward 1, Forward 3, Down 6, Down 6, Forward 2, Up 1, Forward 9, Up 1, Forward 9, Down 1, Forward 2, Forward 3, Up 3, Down 7, Forward 6, Up 4, Forward 5, Up 4, Forward 4, Down 6, Down 9, Down 8, Down 1, Forward 8, Up 4, Forward 6, Down 8, Down 7, Down 9, Forward 7, Forward 4, Down 4, Forward 8, Up 4, Down 7, Down 1, Down 7, Up 6, Forward 3, Down 8, Down 6, Down 5, Down 7, Down 5, Forward 3, Forward 5, Down 2, Down 8, Up 4, Forward 9, Down 5, Down 1, Forward 6, Down 2, Down 6, Down 3, Up 3, Up 5, Forward 8, Up 2, Down 4, Down 5, Up 4, Forward 1, Forward 2, Up 4, Forward 7, Forward 2, Forward 4, Forward 6, Down 1, Down 9, Up 2, Down 7, Down 6, Up 1, Up 2, Forward 7, Forward 9, Forward 4, Forward 6, Down 4, Up 7, Up 2, Forward 3, Down 8, Up 5, Up 7, Down 4, Down 4, Forward 1, Forward 8, Forward 4, Forward 1, Up 8, Down 3, Down 5, Down 7, Up 2, Forward 6, Down 6, Down 8, Forward 2, Up 7, Down 6, Down 6, Up 4, Up 6, Up 4, Down 3, Forward 9, Up 4, Forward 8, Forward 7, Down 5, Down 4, Down 3, Forward 7, Forward 3, Up 7, Forward 5, Down 2, Forward 4, Forward 3, Forward 1, Down 9, Up 2, Up 3, Up 7, Up 6, Forward 1, Up 3, Down 3, Up 9, Forward 2, Forward 7, Forward 6, Forward 2, Forward 9, Forward 9, Forward 5, Up 2, Down 6, Down 3, Down 2, Forward 7, Down 4, Forward 1, Up 7, Forward 8, Down 5, Down 6, Down 7, Up 5, Forward 6, Forward 5, Up 5, Up 6, Down 4, Up 8, Up 3, Forward 9, Down 4, Down 4, Down 7, Up 7, Down 8, Down 7, Forward 2, Forward 9, Down 2, Down 1, Forward 5, Down 2, Forward 7, Down 5, Down 4, Down 7, Forward 9, Forward 2, Down 6, Forward 8, Down 6, Down 6, Up 8, Forward 9, Up 4, Down 9, Forward 7, Up 1, Up 2, Forward 9, Down 9, Down 6, Down 5, Forward 2, Down 9, Down 1, Forward 1, Down 7, Down 6, Up 6, Down 4, Forward 9, Up 5, Down 3, Down 9, Forward 5, Down 2, Forward 1, Forward 4, Forward 1, Forward 1, Forward 4, Down 2, Up 3, Forward 9, Down 5, Down 2, Forward 5, Down 6, Down 4, Forward 9, Forward 3, Forward 4, Forward 9, Forward 5, Forward 3, Down 5, Up 9, Down 5, Forward 8, Down 9, Forward 7, Down 3, Up 3, Down 7, Up 2, Forward 5, Forward 3, Up 7, Down 1, Forward 2, Down 9, Down 5, Down 2, Forward 6, Forward 6, Forward 5, Down 5, Down 1, Down 4, Down 7, Forward 4, Forward 3, Forward 1, Forward 4, Down 1, Up 7, Up 5, Forward 2, Up 3, Down 2, Forward 2, Forward 8, Down 7, Forward 9, Forward 8, Down 4, Down 5, Forward 4, Forward 7, Up 9, Down 5, Forward 4, Down 7, Forward 5, Down 8, Forward 5, Forward 2, Forward 7, Forward 3, Forward 1, Forward 2, Up 1, Up 5, Up 1, Up 3, Down 9, Up 9, Down 8, Forward 4, Down 3, Forward 7, Down 6, Forward 1, Down 7, Up 3, Forward 1, Forward 6, Up 9, Down 6, Forward 3, Down 1, Forward 7, Down 9, Up 3, Up 9, Forward 6, Up 1, Forward 5, Forward 7, Forward 7, Up 7, Down 2, Up 7, Down 8, Forward 7, Up 5, Down 9, Up 1, Forward 4, Forward 4, Forward 9, Down 6, Up 3, Down 8, Down 8, Up 2, Down 8, Down 8, Up 7, Down 8, Up 2, Up 4, Up 1, Forward 7, Forward 9, Forward 9, Down 4, Up 8, Forward 9, Down 9, Up 1, Forward 1, Forward 1, Down 5, Up 7, Down 8, Forward 4, Forward 3, Down 7, Forward 8, Up 2, Down 2, Down 6, Down 4, Forward 9, Forward 7, Down 9, Down 4, Forward 8, Down 5, Forward 7, Down 2, Forward 6, Up 8, Forward 3, Down 5, Forward 2, Forward 6, Down 9, Up 6, Up 9, Up 2, Forward 2, Down 2, Forward 5, Down 7, Down 8, Down 4, Down 5, Forward 6, Forward 2, Up 9, Down 3, Forward 3, Up 8, Forward 2, Down 9, Forward 4, Forward 1, Forward 1, Up 3, Up 9, Forward 6, Down 5, Down 2, Up 2, Up 9, Forward 9, Forward 6, Forward 3, Forward 9, Up 3, Forward 9, Up 4, Up 5, Forward 6, Forward 6, Down 8, Forward 5, Down 9, Up 5, Forward 5, Down 8, Down 3, Up 8, Down 2, Forward 4, Forward 6, Up 4, Down 3, Down 3, Down 5, Up 8, Down 7, Down 4, Forward 9, Forward 2, Down 1, Down 8, Forward 8, Up 9, Forward 3, Down 2, Up 8, Down 9, Up 2, Down 3, Forward 8, Forward 8, Forward 2, Down 3, Forward 7, Down 7, Down 4, Forward 9, Forward 9, Down 5, Up 7, Forward 2, Up 7, Up 1, Forward 4, Up 2, Down 4, Down 7, Down 4, Down 1, Up 3, Down 5, Down 5, Forward 6, Forward 2, Down 2, Forward 9, Down 4, Up 8, Forward 9, Forward 9, Up 7, Forward 4, Forward 9, Forward 2, Forward 2, Forward 1, Forward 6, Down 3, Down 5, Forward 7, Up 4, Forward 9, Forward 9, Up 1, Forward 9, Down 5, Up 2, Up 2, Down 5, Down 5, Forward 7, Down 1, Forward 5, Up 8, Up 9, Down 9, Forward 3, Up 1, Forward 6, Down 8, Down 2, Forward 6, Up 9, Down 3, Down 1, Down 1, Up 2, Up 1, Up 9, Forward 5, Forward 6, Down 7, Forward 1, Down 3, Forward 6, Forward 2, Forward 9, Forward 7, Up 5, Down 4, Down 6, Down 2, Down 5, Forward 7, Up 1, Down 7, Down 4, Down 7, Down 4, Forward 2, Forward 8, Up 7, Up 9, Down 6, Up 8, Up 3, Up 3, Up 5, Down 1, Forward 4, Forward 9, Forward 1, Down 9, Up 5, Down 3, Down 1, Down 1, Up 3, Down 1, Up 2, Up 2, Down 5, Forward 8, Down 8, Up 6, Forward 9, Up 1, Up 3, Down 4, Forward 7, Up 4, Forward 3, Down 8, Forward 6, Down 6, Forward 1, Down 4, Down 7, Up 3, Down 4, Forward 5, Forward 4, Down 6, Up 4, Down 6, Up 8, Forward 1, Up 8, Forward 3, Down 8, Forward 3, Down 9, Forward 5, Down 6, Forward 5, Forward 7, Forward 8, Down 8, Down 1, Down 1, Down 7, Forward 9, Down 4, Forward 3, Down 8, Down 4, Down 6, Down 8, Forward 5, Down 5, Down 1, Down 9, Down 6, Down 7, Down 1, Forward 7, Down 2, Down 9, Down 9, Down 8, Down 9, Forward 9, Down 8, Forward 1, Up 2, Forward 4, Up 2, Up 7, Forward 1, Down 9, Up 7, Forward 4, Forward 2, Down 2, Down 5, Down 7, Down 4, Forward 8, Up 2, Forward 1, Forward 5, Down 7, Forward 3, Forward 6, Down 3, Up 5, Up 8, Down 5, Down 1, Down 7, Down 6, Forward 2, Forward 3, Forward 7, Forward 6, Down 2, Down 4, Down 1, Down 5, Down 4, Down 7, Up 5, Down 4, Up 9, Forward 7, Down 9, Down 9, Forward 3, Forward 9, Down 5, Forward 1, Up 1, Down 4, Forward 7, Up 4, Down 5, Forward 8, Forward 3, Forward 6, Forward 7, Down 8, Down 3, Forward 8, Down 8, Forward 7, Down 4, Down 2, Down 8, Down 3, Forward 4, Down 5, Up 5, Down 9, Up 5, Up 4, Up 3, Forward 7, Forward 8, Forward 9, Forward 5, Down 7, Down 2, Forward 2, Down 2, Forward 5, Forward 2, Forward 6, Down 4, Down 5, Down 7, Forward 3, Forward 3, Forward 9, Forward 6, Down 2, Forward 3, Down 5, Forward 5, Forward 9, Forward 6, Up 4, Forward 6, Forward 9, Down 3, Down 2, Forward 9, Down 1, Up 1, Forward 1, Up 3, Forward 3, Forward 6, Up 4, Up 4, Forward 8, Forward 1, Forward 2]
