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
