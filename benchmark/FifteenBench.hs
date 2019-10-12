module FifteenBench where

import Fifteen
import Data.Vector
import Criterion.Main

main :: IO ()
main = defaultMain groups
  where
    groups = [bgroup "a* solver" $ solverGroups1 <> solverGroups2]

    solverGroups1 = do
      (id, state) <- [ ("state 1", fromList $ [T1 .. T12] <> [T15, T13, Blank, T14])
                     , ("state 2", fromList $ [T1 .. T8] <> [T15, T14, T13, Blank] <> [T12, T11 .. T9])
                     ]
      pure . bgroup id $ do
        (hid, heuristic) <- [ ("manhattan", manhattanScore)
                            , ("manhattan, weight 3", (3 *) . manhattanScore)
                            , ("manhattan, weight 10", (10 *) . manhattanScore)
                            , ("linear conflict", linearConflictScore)
                            , ("linear conflict, weight 3", (3 *) . linearConflictScore)
                            , ("linear conflict, weight 10", (10 *) . linearConflictScore)
                            ]
        pure . bench hid $ whnf (solve heuristic) state

    solverGroups2 =
      pure . bgroup "state 3" $ do
      let state = fromList $ [Blank, T2, T1] <> [T3 .. T15]
      weight <- [9,15]
      pure . bench ("linear conflict, weight " <> show weight) $ whnf (solve $ (weight *) . linearConflictScore) state
