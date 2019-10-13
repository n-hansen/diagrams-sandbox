module FifteenMain where

import Fifteen
import Data.Vector

import Diagrams.Backend.Cairo.CmdLine


main :: IO ()
main =
  animMain d
  --renderCairo out sz d
  where
    d = renderSolution exampleState
    exampleState =
      fromList $ [T1 .. T8] <> [T15, T14, T13, Blank] <> [T12, T11 .. T9]
      -- fromList $ [Blank, T2, T1] <> [T3 .. T15]
