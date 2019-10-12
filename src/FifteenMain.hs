module FifteenMain where

import Fifteen
import Data.Vector

main :: IO ()
main =
  print $ fmap fst <$> solve ((8 *) . manhattanScore) exampleState
  where
    exampleState =
      fromList $ [T1 .. T8] <> [T15, T14, T13, Blank] <> [T12, T11 .. T9]
