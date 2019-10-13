module FifteenMain where

import Fifteen

import Diagrams.Backend.Cairo.CmdLine

main :: IO ()
main = animation >>= animMain
  where
    animation =
      fmap movie
      . sequence
      . take 2
      $ repeat renderShuffleThenSolve
