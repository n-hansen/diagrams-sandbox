{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Fifteen (
  PuzzleState,
  Tile(..),
  Move(..),
  ix2coord,
  coord2ix,
  isSolvable,
  solve,
  renderPuzzle,
  renderMove,
  renderSolution,
  renderShuffleThenSolve,
  applyMove,
  manhattanScore,
  linearConflictScore
  ) where

import           Control.Monad.Random
import           Control.Monad.Writer
import           Data.Colour.Palette.ColorSet
import           Data.Colour.RGBSpace (uncurryRGB)
import           Data.Colour.RGBSpace.HSL
import qualified Data.IntMap.Strict     as IM
import qualified Data.Map.Strict        as M
import qualified Data.Set               as S
import           Data.STRef
import qualified Data.Vector            as V
import           Data.Vector.Instances()
import qualified Data.Vector.Mutable    as VM
import qualified Ease
import           System.Random.Shuffle


type PuzzleState = V.Vector Tile

data Tile = T1  | T2  | T3  | T4
          | T5  | T6  | T7  | T8
          | T9  | T10 | T11 | T12
          | T13 | T14 | T15 | Blank
          deriving (Eq,Show,Enum,Ord)

data Move = Move Int Int deriving Show

stateId :: PuzzleState -> Word64
stateId = V.foldl' (\acc t -> acc * 16 + (fromIntegral . fromEnum) t) 0

ix2coord :: Int -> (Int,Int)
ix2coord ix = swap $ divMod ix 4

coord2ix :: (Int,Int) -> Int
coord2ix (x,y) = x + y * 4

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
                mid = (lo + hi) `div` 2
            -- set pivot as median of lo/mid/hi
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

        emptySpaceParity = maybe 0 (uncurry (+) . ix2coord) . V.findIndex (== Blank) $ ps

solve :: (PuzzleState -> Int) -> PuzzleState -> Maybe [(Move, PuzzleState)]
solve score ps = if isSolvable ps then findSolution else Nothing
  where
    findSolution =
      reverse <$>
      go (S.singleton . stateId $ ps) (IM.singleton 0 [[(Move 0 0, ps)]])

    go :: Set Word64 -> IntMap [[(Move, PuzzleState)]] -> Maybe [(Move, PuzzleState)]
    go seenStates prioritizedTree = do
      currentPath@((_, currentNode):_) <- head =<< (snd <$> IM.lookupMin prioritizedTree)
      let depth = length currentPath
          children =
            filter (flip S.notMember seenStates . stateId . snd)
            $ expandNode currentNode
          solution = find (isSolved . snd) children
          seenStates' =
            S.union seenStates
            . S.fromList
            . fmap (stateId . snd)
            $ children
          prioritizedTree' =
            IM.unionsWith (<>)
            . (IM.updateMin (tailMay >=> guarded (not . null)) prioritizedTree :)
            . fmap (\c@(_, cNode) -> IM.singleton (depth + 1 + score cNode) [c:currentPath])
            $ children
      (: currentPath) <$> solution
        <|> go seenStates' prioritizedTree'

    isSolved :: PuzzleState -> Bool
    isSolved st =
      V.and
      . V.zipWith (<) st
      $ V.tail st

    expandNode :: PuzzleState -> [(Move, PuzzleState)]
    expandNode st = do
      blankAt <- maybeToList . V.findIndex (== Blank) $ st
      let (x,y) = ix2coord blankAt
      fmap (\m -> (m, applyMove m st))
        . catMaybes
        $ [ if x > 0 then Just $ Move (blankAt - 1) blankAt else Nothing
          , if x < 3 then Just $ Move (blankAt + 1) blankAt else Nothing
          , if y > 0 then Just $ Move (blankAt - 4) blankAt else Nothing
          , if y < 3 then Just $ Move (blankAt + 4) blankAt else Nothing
          ]

manhattanScore :: PuzzleState -> Int
manhattanScore =
  flip V.ifoldl' 0 $
  \acc ix t ->
    case t of
      Blank -> 0
      _     ->
        let (x1,y1) = ix2coord ix
            (x2,y2) = ix2coord $ fromEnum t
        in acc + abs (x1 - x2) + abs (y1 - y2)

linearConflictScore :: PuzzleState -> Int
linearConflictScore st = manhattanScore st + 2 * linearConflicts
  where
    linearConflicts = sum . fmap countConflicts $ rows

    countConflicts [] = 0
    countConflicts (x:xs) = (countConflicts xs +) . length . filter (< x) $ xs

    rows = fmap V.toList . unfoldr (\ts -> if V.null ts then Nothing else Just $ V.splitAt 4 ts) $ st

applyMove :: Move -> PuzzleState -> PuzzleState
applyMove (Move from to) st =
  st V.// [ (from, Blank)
          , (to, st V.! from)
          ]

renderPuzzle ps = vsep spacing rows
  where
    spacing = 0.1

    rows = fmap (renderRow . V.toList)
           . unfoldr (\ts -> if V.null ts then Nothing else Just $ V.splitAt 4 ts)
           $ ps

    renderRow = hsep spacing . fmap renderTile

    renderTile t =
      case t of
        Blank -> square 1
        _     -> square 1 <> text (show . (+ 1) . fromEnum $ t)

uiInterpolate :: Fractional a => a -> a -> Active a
uiInterpolate start end =
  mkActive 0 1 $ \t -> start + (end - start) * (fromRational . fromTime) t

uiEased :: (Floating a, Ord a) => a -> a -> Active a
uiEased start end =
  mkActive 0 1 $ \t -> start + (end - start) * (Ease.quadInOut . fromRational . fromTime) t

hueBlend :: (Ord a, RealFrac a, Floating a) => a -> Colour a -> Colour a -> Colour a
hueBlend pct start end = uncurryRGB sRGB $ hsl hBlend (mix s0 s1) (mix l0 l1)
  where
    (h0, s0, l0) = hslView . toSRGB $ start
    (h1, s1, l1) = hslView . toSRGB $ end
    mix x0 x1 = x0 + pct * (x1 - x0)
    hBlend = if abs (h0 - h1) <= 0.5 then mix h0 h1
             else let mixed = if h0 > h1
                              then mix h0 (h1 + 1)
                              else mix (h0 + 1) h1
                  in if mixed <= 1 then mixed else mixed - 1

renderMove initialColors pctStart pctEnd ps (Move fromIx toIx) = dynamic <> static
  where
    gridScale = 2
    padding = 0.1
    baseTile = square $ gridScale - padding

    static =
      vsep padding
      . fmap renderRow
      . unfoldr (\ts -> if V.null ts then Nothing else Just $ V.splitAt 4 ts)
      . V.imap (\ix t -> guard (ix /= fromIx && ix /= toIx) >> pure t)
      $ ps

    renderRow =
      hsep padding
      . fmap renderTile
      . V.toList

    tileColor t =
      let ix = fromEnum t
      in rybColor $ ix + 2*(snd . ix2coord $ ix)

    renderTile t =
      case t of
        Nothing ->
          pure $ baseTile # lw none
        Just Blank ->
          pure $ baseTile # lw none
        Just t' ->
          let fromColor = tileColor $ initialColors M.! t'
              toColor = tileColor t'
              c = hueBlend <$> uiInterpolate pctStart pctEnd <*> pure fromColor <*> pure toColor
          in fc <$> c <*> baseTile

    coords = first ((gridScale *) . fromIntegral) . second (negate . (gridScale *) . fromIntegral) . ix2coord
    (fromX, fromY) = coords fromIx
    (toX, toY) = coords toIx

    dynamic =
      let tx = translateX <$> uiEased fromX toX
          ty = translateY <$> uiEased fromY toY
          tile = renderTile $ ps V.!? fromIx
      in tx <*> (ty <*> tile)

renderSolution initialColors ps = movie . renderMoves 0 $ solution
  where
    solution = fromMaybe [] $ solve ((13 *) . linearConflictScore) ps

    stepSize = 1 / fromIntegral (traceShowId $ length solution)

    renderMoves _ [] = []
    renderMoves _ [_] = []
    renderMoves i ((_,st):rest@((mv,_):_)) =
      renderMove initialColors (Ease.sineInOut $ i*stepSize) (Ease.sineInOut $ (i+1)*stepSize) st mv : renderMoves (traceShowId $ i+1) rest

renderShuffleThenSolve = do
  let solvableShuffle = do
        s <- V.fromList . (<> [Blank]) <$> shuffleM [T1 .. T15]
        if isSolvable s then pure s else solvableShuffle
  ps <- evalRandIO solvableShuffle
  let initialColors = M.fromList . zip (V.toList ps) $ [T1 .. T15]
  pure $ renderSolution initialColors ps
