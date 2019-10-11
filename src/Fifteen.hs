module Fifteen where

import           Control.Monad.Writer
import           Data.STRef
import qualified Data.Vector            as V
import qualified Data.Vector.Mutable    as VM
import           Diagrams.Backend.Cairo

type PuzzleState = V.Vector Tile

newtype Tile = Tile (Maybe Int) deriving (Eq, Show)

instance Ord Tile where
  _ <= Tile Nothing = True
  Tile Nothing <= _ = False
  Tile (Just x) <= Tile (Just y) = x <= y

ix2coord :: Int -> (Int,Int)
ix2coord x = divMod x 4

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
