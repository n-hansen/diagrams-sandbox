module FifteenBench where

import Fifteen
import Data.Vector
import Criterion.Main

main :: IO ()
main = defaultMain groups
  where
    groups = do
      (id, state) <- [ ("state 1", fromList $ [T1 .. T12] <> [T15, T13, Blank, T14])
                     , ("state 2", fromList $ [T1 .. T8] <> [T15, T14, T13, Blank] <> [T12, T11 .. T9])
                     ]
      pure . bgroup id $ do
        (hid, heuristic) <- [ ("manhattan", manhattanScore)
                            , ("manhattan, weight 3", (3 *) . manhattanScore)
                            , ("manhattan, weight 10", (10 *) . manhattanScore)
                            ]
        pure . bench hid $ whnf (solve heuristic) state
