module FifteenMain where

import Fifteen
import Data.Vector
import Data.Active

import Diagrams.Backend.Cairo.CmdLine


main :: IO ()
main = do
  s1 <- renderShuffleThenSolve
  s2 <- renderShuffleThenSolve
  s3 <- renderShuffleThenSolve
  animMain $ movie [s1,s2,s3]
