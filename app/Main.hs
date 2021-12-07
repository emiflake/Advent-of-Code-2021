module Main where

import Criterion.Main
import Day07 qualified

main =
  defaultMain
    [ bench "Day07" (nfIO Day07.exe)
    ]
