module FifteenMain where

import Fifteen

import Diagrams.Backend.Cairo.CmdLine

main :: IO ()
main = buildAnimationSequence 3 >>= animMain
