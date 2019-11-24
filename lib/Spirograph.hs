{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-partial-type-signatures -fno-warn-missing-signatures #-}
module Spirograph where

import           Data.Colour.RGBSpace (uncurryRGB)
import           Data.Colour.RGBSpace.HSL
import qualified Data.Map.Strict as M




data Placement = Placement { fixedContactPoint :: P2 Double
                           , rollingContactPoint :: P2 Double
                           , rollingRotation :: Angle Double
                           }
               -- | BoundaryPlacement { fixedContactPoint1 :: P2 Double
               --                     , rollingContactPoint1 :: P2 Double
               --                     , rollingRotation1 :: Angle Double
               --                     , fixedContactPoint2 :: P2 Double
               --                     , rollingContactPoint2 :: P2 Double
               --                     , rollingRotation2 :: Angle Double
               --                     }deriving Show

computePlacement :: Located (Trail V2 Double)-> Double -> Located (Trail V2 Double) -> Double -> Double -> Placement
computePlacement fixedTrail fixedOffset rollingTrail rollingOffset distance
  | shouldWrap && fixedDistanceLeft < rollingDistanceLeft = computePlacement
                                                            fixedTrail 0
                                                            rollingTrail (rollingOffset + fixedDistanceLeft)
                                                            (distance - fixedDistanceLeft)
  | shouldWrap = computePlacement
                 fixedTrail (fixedOffset + rollingDistanceLeft)
                 rollingTrail 0
                 (distance - rollingDistanceLeft)
  | otherwise = Placement fixedContactPoint rollingContactPoint rollingRotation
  where
    fixedDistanceLeft = stdArcLength fixedTrail - fixedOffset
    rollingDistanceLeft = stdArcLength rollingTrail - rollingOffset
    shouldWrap = fixedDistanceLeft < distance || rollingDistanceLeft < distance
    fixedParam = stdArcLengthToParam fixedTrail (fixedOffset + distance)
    fixedContactPoint = fixedTrail `atParam` fixedParam
    fixedTangent = tangentAtParam fixedTrail fixedParam
    rollingParam = stdArcLengthToParam rollingTrail (rollingOffset + distance)
    rollingContactPoint = rollingTrail `atParam` rollingParam
    rollingTangent = tangentAtParam rollingTrail rollingParam
    rollingRotation = signedAngleBetween fixedTangent rollingTangent

placeAt :: _ => _ -> Placement -> _
placeAt thing Placement{fixedContactPoint, rollingContactPoint, rollingRotation} =
  thing
  # translate (origin .-. rollingContactPoint)
  # rotate rollingRotation
  # translate (fixedContactPoint .-. origin)

spirograph' :: Located (Trail V2 Double) -> Located (Trail V2 Double) -> P2 Double -> Double -> Double -> [P2 Double]
spirograph' fixedCurve rollingCurve penLocation distance stepSize = curve
  where
    placePen x = penLocation `placeAt` computePlacement fixedCurve 0 rollingCurve 0 x
    curve = placePen <$> [0,stepSize..distance]

spirograph f r p d s = fromVertices $ spirograph'' f r p d s

data TrailTraversal = TT { currSegment :: Located (Segment Closed V2 Double)
                         , currSegmentIx :: Int
                         , currDistLeft :: Double
                         , nextSegments :: [(Int,Located (Segment Closed V2 Double), Double)]
                         }

initTrailTraversal :: Located (Trail V2 Double) -> TrailTraversal
initTrailTraversal curve = TT initSeg initIx initDist segs
  where
    (initIx,initSeg,initDist):segs = cycle
                                     . fmap (\(ix,seg) -> (ix,seg,stdArcLength seg))
                                     . zip [0..]
                                     . trailLocSegments
                                     $ curve

stepTrailTraversalBy :: TrailTraversal -> Double -> (Located (Segment Closed V2 Double), Int, Double, TrailTraversal)
stepTrailTraversalBy tt@TT{currSegment,currSegmentIx,currDistLeft,nextSegments} delta =
  if delta < currDistLeft
    then (currSegment, currSegmentIx, currDistLeft', tt {currDistLeft = currDistLeft'})
    else (nextSegment, nextSegmentIx, nextDistLeft', TT nextSegment nextSegmentIx nextDistLeft' segs)
  where
    currDistLeft' = currDistLeft - delta
    (nextSegmentIx, nextSegment, nextDistLeft):segs = nextSegments
    nextDistLeft' = nextDistLeft + currDistLeft'

spirograph'' :: Located (Trail V2 Double) -> Located (Trail V2 Double) -> P2 Double -> Double -> Double -> [P2 Double]
spirograph'' fixedCurve rollingCurve penLocation distance stepSize = placeAt penLocation <$> placements
  where
    samplePoints :: [(Double, Located (Segment Closed V2 Double), Int, Double, Located (Segment Closed V2 Double), Int, Double)]
    samplePoints = buildSamplePoints 0 distance (initTrailTraversal fixedCurve) (initTrailTraversal rollingCurve)

    advanceAndBuildSamplePoints toGo fixedTraversal rollingTraversal =
      let advancement = min toGo $ minimum [currDistLeft fixedTraversal, currDistLeft rollingTraversal, stepSize]
      in buildSamplePoints advancement (toGo - advancement) fixedTraversal rollingTraversal

    buildSamplePoints advancement toGo fixedTraversal rollingTraversal =
      let (fSeg, fSegIx, fDistLeft, fTrav) = stepTrailTraversalBy fixedTraversal advancement
          (rSeg, rSegIx, rDistLeft, rTrav) = stepTrailTraversalBy rollingTraversal advancement
      in (toGo, fSeg, fSegIx, fDistLeft, rSeg, rSegIx, rDistLeft) : if toGo <= 0 then [] else advanceAndBuildSamplePoints toGo fTrav rTrav

    segmentPointTable = M.fromList
                        . concatMap computeSegmentPoints
                        . groupBy (on (==) $ \(ix,_,_,_) -> ix)
                        . sortOn (\(ix,_,_,d) -> (ix,-d))
                        . concatMap (\(ptId,fSeg,fSegIx,fDistLeft,rSeg,rSegIx,rDistLeft) -> [ (Left fSegIx, ptId, fSeg, fDistLeft)
                                                                                            , (Right rSegIx, ptId, rSeg, rDistLeft)
                                                                                            ]
                                    )
                        $ samplePoints

    computeSegmentPoints [] = []
    computeSegmentPoints pts'@((ix, _, fullSeg,_):_) =
      let fullSegLen = stdArcLength fullSeg
          go [] _ _ _ = []
          go allPts@((ptId,pt):pts) allSegs@((seg,segLen,segPLen):segs) p d =
            let entryAtParam atP = ((ix, ptId), (seg `atParam` atP, seg `tangentAtParam` atP))
                closeEnough x = abs (pt - x) < stdTolerance
            in if | closeEnough d  -> entryAtParam p : go pts allSegs p d
                  | d + segLen < pt -> go allPts segs (p+segPLen) (d+segLen)
                  | closeEnough $ d + segLen -> entryAtParam (p+segPLen) : go pts segs (p+segPLen) (d+segLen)
                  | otherwise -> let (l,h) = seg `splitAtParam` 0.5
                                     lLen = stdArcLength l
                                     hLen = stdArcLength h
                                 in -- traceShow (pt, segLen, p, d) $
                                    go allPts ((l,lLen,segPLen/2):(h,hLen,segPLen/2):segs) p d
      in go
         (fmap (\(_,ptId,_,d) -> (ptId,fullSegLen - d)) pts')
         [(fullSeg, fullSegLen, 1)]
         0 0

    placements = do
      (ptId, _, fSegIx, _, _, rSegIx, _) <- samplePoints
      (fPt, fTan) <- maybeToList $ M.lookup (Left fSegIx, ptId) segmentPointTable
      (rPt, rTan) <- maybeToList $ M.lookup (Right rSegIx, ptId) segmentPointTable
      pure $ Placement fPt rPt $ signedAngleBetween fTan rTan




example :: _ => QDiagram _ V2 Double Any
example = curve
          # explodePath
          # mconcat
          # zipWith lc [uncurryRGB sRGB $ hsl h 1 0.6 | h <- [0,1..]]
          # mconcat
  where
    f = circle 2
    r = circle 1.1 # translateY 0.288
    curve = spirograph f r origin 60 0.1

example2 :: IO ()
example2 = traverse_ {-   (\(d,_,fix,flen,_,six,slen)-> print $(d,fix,flen,six,slen)) -} print $ sort curve
  where
    f = circle 2
    r = circle 1.1 # translateY 0.288
    curve = spirograph'' f r origin 20 0.8


test = undefined
