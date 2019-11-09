{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
import Criterion.Main

import Fifteen
import Spirograph
import Data.Vector

main :: IO ()
main = defaultMain groups
  where
    groups = [ fifteenGroup
             , spirographGroup
             ]

    fifteenGroup = bgroup "fifteen-solver" $ solverGroups1 <> solverGroups2

    solverGroups1 = do
      (id, state) <- [ ("state1", fromList $ [T1 .. T12] <> [T15, T13, Blank, T14])
                     , ("state2", fromList $ [T1 .. T8] <> [T15, T14, T13, Blank] <> [T12, T11 .. T9])
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
      pure . bgroup "state3" $ do
      let state = fromList $ [Blank, T2, T1] <> [T3 .. T15]
      weight <- [9,15]
      pure . bench ("linear conflict, weight " <> show weight) $ whnf (solve $ (weight *) . linearConflictScore) state

    spirographGroup = bgroup "spirograph"
                      [ bench "render two circles"
                        $ nf ( spirograph' (circle 2) (circle 1.1 # translate 0.288) origin 10) 0.5
                      ]
