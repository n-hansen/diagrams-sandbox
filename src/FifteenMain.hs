module FifteenMain where

import Fifteen
import Data.Vector

main :: IO ()
main =
  print $ fmap fst <$> solve ((15 *) . linearConflictScore) exampleState
  where
    exampleState =
      --fromList $ [T1 .. T8] <> [T15, T14, T13, Blank] <> [T12, T11 .. T9]
      fromList $ [Blank, T2, T1] <> [T3 .. T15]
