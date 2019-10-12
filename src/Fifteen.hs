{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Fifteen where

import           Control.Monad.Writer
import qualified Data.IntMap.Strict     as IM
import qualified Data.IntSet            as IS
import           Data.STRef
import qualified Data.Vector            as V
import           Data.Vector.Instances()
import qualified Data.Vector.Mutable    as VM
import           Diagrams.Backend.Cairo

type PuzzleState = V.Vector Tile

newtype Tile = Tile (Maybe Int) deriving (Eq, Show, Hashable)

instance Ord Tile where
  _ <= Tile Nothing = True
  Tile Nothing <= _ = False
  Tile (Just x) <= Tile (Just y) = x <= y

data Move = Move Int Int deriving Show

ix2coord :: Int -> (Int,Int)
ix2coord ix = divMod ix 4

coord2ix :: (Int,Int) -> Int
coord2ix (x,y) = x * 4 + y

isSolvable :: PuzzleState -> Bool
isSolvable ps = even $ permutationParity + emptySpaceParity
    where
        permutationParity =
          getSum
          $ runST
          $ execWriterT
          $ lift (V.thaw ps) >>= qsort 0 (V.length ps - 1)

        qsort lo hi vec =
          if lo >= hi then return ()
          else do
            -- some shorthand
            let read i = lift $ VM.read vec i
                swap i j =
                  when (i /= j) $ do
                  tell 1
                  lift $ VM.swap vec i j
                -- set pivot as median of lo/mid/hi
                mid = (lo + hi) `div` 2
            whenM ((<) <$> read mid <*> read lo) $
              swap lo mid
            whenM ((<) <$> read hi <*> read lo) $
              swap lo hi
            whenM ((<) <$> read mid <*> read hi) $
              swap mid hi
            pivot <- read hi
            i <- lift $ newSTRef lo
            forM_ [lo..hi] $ \j -> do
              vec_j <- read j
              when (vec_j < pivot) $ do
                i' <- lift $ readSTRef i
                swap i' j
                lift $ modifySTRef i (+ 1)
            i' <- lift $ readSTRef i
            swap i' hi
            _ <- qsort lo (i' - 1) vec
            qsort (i' + 1) hi vec

        emptySpaceParity = maybe 0 (uncurry (+) . ix2coord) . V.findIndex (== Tile Nothing) $ ps

solve :: PuzzleState -> Maybe [(Move, PuzzleState)]
solve ps = if isSolvable ps then findSolution else Nothing
  where
    findSolution =
      reverse <$>
      go (IS.singleton . hash $ ps) (IM.singleton 0 [[(Move 0 0, ps)]])

    go :: IntSet -> IntMap [[(Move, PuzzleState)]] -> Maybe [(Move, PuzzleState)]
    go seenStates prioritizedTree = do
      currentPath@((_, currentNode):_) <- head =<< (snd <$> IM.lookupMin prioritizedTree)
      let depth = length currentPath
          children =
            filter (flip IS.notMember seenStates . hash . snd)
            $ expandNode currentNode
          solution = find (isSolved . snd) children
          seenStates' =
            IS.union seenStates
            . IS.fromList
            . fmap (hash . snd)
            $ children
          prioritizedTree' =
            IM.unionsWith (<>)
            . (IM.updateMin (tailMay >=> guarded (not . null)) prioritizedTree :)
            . fmap (\c@(_, cNode) -> IM.singleton (depth + 1 + score cNode) [c:currentPath])
            $ children
      (: currentPath) <$> solution
        <|> go seenStates' prioritizedTree'

    score :: PuzzleState -> Int
    score = flip V.ifoldl' 0 $
      \acc ix (Tile t) ->
        case t of
          Nothing -> 0
          Just n  ->
            let (x1,y1) = ix2coord ix
                (x2,y2) = ix2coord $ n - 1
            in acc + (abs $ x1 - x2) + (abs $ y1 - y2)

    isSolved :: PuzzleState -> Bool
    isSolved st =
      V.and
      . V.zipWith (<) st
      $ V.tail st

    expandNode :: PuzzleState -> [(Move, PuzzleState)]
    expandNode st = do
      blankAt <- maybeToList . V.findIndex (== Tile Nothing) $ st
      let (x,y) = ix2coord blankAt
      fmap (\m -> (m, applyMove m st))
        . catMaybes
        $ [ if x > 0 then Just $ Move (blankAt - 4) blankAt else Nothing
          , if x < 3 then Just $ Move (blankAt + 4) blankAt else Nothing
          , if y > 0 then Just $ Move (blankAt - 1) blankAt else Nothing
          , if y < 3 then Just $ Move (blankAt + 1) blankAt else Nothing
          ]

applyMove :: Move -> PuzzleState -> PuzzleState
applyMove (Move from to) st =
  st V.// [ (from, Tile Nothing)
          , (to, st V.! from)
          ]

renderPuzzle ps = vsep spacing rows
  where
    spacing = 0.1

    rows = fmap (renderRow . V.toList)
           . unfoldr (\ts -> if V.null ts then Nothing else Just $ V.splitAt 4 ts)
           $ ps

    renderRow = hsep spacing . fmap renderTile

    renderTile (Tile t) =
      case t of
        Nothing -> square 1
        Just n  -> square 1 <> text (show n)


main = renderCairo out sz d
  where
    out = "out/fifteen.png"
    sz  = mkSizeSpec2D (Just 500) Nothing
    d   = renderPuzzle state1

state1 = V.fromList . map Tile . (Nothing :) . map Just $ [1..15]

state2 = V.fromList . map Tile . (Nothing :) . map Just $ [2,1] <> [3..15]

state3 = V.fromList . map Tile $ [Just x | x <- [1..12]] <> [Just 15, Just 13, Nothing, Just 14]
