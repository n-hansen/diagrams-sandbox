module SpirographMain where

import Spirograph

import Diagrams.Backend.Cairo.CmdLine

main :: IO ()
main = mainWith d
  where
    d :: Diagram B
    d = example
    -- f = circle 3
    -- r = circle 2 # translateX 2
    -- s = spirograph f r # strokePath # lc blue
    -- d :: Diagram B
    -- d = s `atop` f
