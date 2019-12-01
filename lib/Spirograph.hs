{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-partial-type-signatures -fno-warn-missing-signatures #-}
module Spirograph
  ( spirograph
  , spirographPoints
  , example
  ) where

import           Control.Arrow
import           Data.Colour.RGBSpace     (uncurryRGB)
import           Data.Colour.RGBSpace.HSL


-- | Information for how to transform rolling coordinates to fixed coordinates
data Placement = Placement { fixedContactPoint   :: P2 Double
                           , rollingContactPoint :: P2 Double
                           , rollingRotation     :: Angle Double
                           }
               -- -- | BoundaryPlacement { fixedContactPoint1 :: P2 Double
               --                     , rollingContactPoint1 :: P2 Double
               --                     , rollingRotation1 :: Angle Double
               --                     , fixedContactPoint2 :: P2 Double
               --                     , rollingContactPoint2 :: P2 Double
               --                     , rollingRotation2 :: Angle Double
               --                     }deriving Show

placeAt :: _ => _ -> Placement -> _
placeAt thing Placement{fixedContactPoint, rollingContactPoint, rollingRotation} =
  thing
  # translate (origin .-. rollingContactPoint)
  # rotate rollingRotation
  # translate (fixedContactPoint .-. origin)

spirograph f r p d s = cubicSpline False $ spirographPoints f r p d s

-- | State store for a traversal of a trail
data TrailTraversal = TT { ttCurrSegment       :: Located (Segment Closed V2 Double)
                         , ttCurrSegmentLength :: Double
                         , ttCurrSegmentIx     :: SegmentId
                         , ttCurrDistLeft      :: Double
                         , ttNextSegments      :: [(SegmentId, Located (Segment Closed V2 Double), Double)]
                         }

-- | Each sampled point corresponds to a specific distance (from the end) of our traversal.
newtype SampleId = SmpId { unSmpId :: Double } deriving (Eq, Show)
instance Ord SampleId where
  compare = compare `on` negate . unSmpId

newtype SegmentId = SegId Int deriving (Eq, Ord, Enum, Show)

-- | Point on a specific segment that we want to sample at.
data SegmentSamplePoint = SSP { sspSegment             :: Located (Segment Closed V2 Double)
                              , sspSegmentLength       :: Double
                              , sspSegmentIx           :: SegmentId
                              , sspSegmentDistanceLeft :: Double
                              , sspSampleId            :: SampleId
                              } deriving Show

-- | Reified sample point from a segment/trail
data ComputedSamplePoint = CSP { cspPoint    :: P2 Double
                               , cspTangent  :: V2 Double
                               , cspSampleId :: SampleId
                               } deriving Show

spirographPoints :: Located (Trail V2 Double) -> Located (Trail V2 Double) -> P2 Double -> Double -> Double -> [P2 Double]
spirographPoints fixedCurve rollingCurve penLocation distance stepSize = placeAt penLocation <$> placements
  where
    placements = uncurry (zipWith computePlacement)
                 . (computeSegmentPoints *** computeSegmentPoints)
                 . unzip
                 $ samplePoints

    samplePoints :: [(SegmentSamplePoint, SegmentSamplePoint)]
    samplePoints = buildSamplePoints 0 distance (initTrailTraversal fixedCurve) (initTrailTraversal rollingCurve)

    advanceAndBuildSamplePoints toGo fixedTraversal rollingTraversal =
      let advancement = min toGo $ minimum [ttCurrDistLeft fixedTraversal, ttCurrDistLeft rollingTraversal, stepSize]
      in buildSamplePoints advancement (toGo - advancement) fixedTraversal rollingTraversal

    buildSamplePoints advancement toGo fixedTraversal rollingTraversal =
      let wrapSSP (seg, segLen, segIx, distLeft, trav) = (SSP seg segLen segIx distLeft (SmpId toGo), trav)
          (fSSP, fTrav) = wrapSSP $ stepTrailTraversalBy fixedTraversal advancement
          (rSSP, rTrav) = wrapSSP $ stepTrailTraversalBy rollingTraversal advancement
      in (fSSP, rSSP) : if toGo <= 0 then [] else advanceAndBuildSamplePoints toGo fTrav rTrav

    initTrailTraversal curve = let (initIx,initSeg,initDist):segs = cycle
                                                                    . fmap (\(ix,seg) -> (ix,seg,stdArcLength seg))
                                                                    . zip [SegId 0..]
                                                                    . trailLocSegments
                                                                    $ curve
                               in TT initSeg initDist initIx initDist segs

    stepTrailTraversalBy tt@TT{ttCurrSegment,ttCurrSegmentLength,ttCurrSegmentIx,ttCurrDistLeft,ttNextSegments} delta =
      let currDistLeft' = ttCurrDistLeft - delta
          (nextSegmentIx, nextSegment, nextSegmentLength):segs = ttNextSegments
          nextDistLeft' = nextSegmentLength + currDistLeft'
      in if delta < ttCurrDistLeft
         then (ttCurrSegment, ttCurrSegmentLength, ttCurrSegmentIx, currDistLeft', tt {ttCurrDistLeft = currDistLeft'})
         else (nextSegment, nextSegmentLength, nextSegmentIx, nextDistLeft', TT nextSegment nextSegmentLength nextSegmentIx nextDistLeft' segs)

    computeSegmentPoints = sortOn cspSampleId
                           . concatMap computeSingleSegmentPoints
                           . groupBy ((==) `on` sspSegmentIx)
                           . sortOn (sspSegmentIx &&& negate . sspSegmentDistanceLeft)

    computeSingleSegmentPoints [] = []
    computeSingleSegmentPoints pts'@(SSP{sspSegment=fullSeg,sspSegmentLength=fullSegLen}:_) =
      let entryAtParam sid p = CSP (fullSeg `atParam` p) (fullSeg `tangentAtParam` p) sid
          go [] _ _ _ = []
          -- because we don't track our error bounds, we can sometimes run into the issue of the subdivided segments having total length
          -- less than some of the offsets we need to compute. the simple, hacky solution is to just approximate any such points as occuring
          -- at the end of the segment, and this seems to give acceptable results.
          go slop [] _ _ = [entryAtParam sspSampleId $ domainUpper fullSeg | SSP{sspSampleId} <- slop]
          go allPts@(SSP{sspSegmentDistanceLeft,sspSampleId}:pts) allSegs@((seg,segLen,segPLen):segs) p d =
            let pt = fullSegLen - sspSegmentDistanceLeft
                closeEnough x = abs (pt - x) < stdTolerance
            in if | closeEnough d  -> entryAtParam sspSampleId p : go pts allSegs p d
                  | d + segLen < pt -> go allPts segs (p+segPLen) (d+segLen)
                  | closeEnough $ d + segLen -> entryAtParam sspSampleId (p+segPLen) : go pts segs (p+segPLen) (d+segLen)
                  | otherwise -> let (l,h) = seg `splitAtParam` 0.5
                                     lLen = stdArcLength l
                                     hLen = -- stdArcLength h
                                            segLen - lLen -- much cheaper than calling stdArcLength, but could grow our error over time...
                                 in -- traceShow (pt, segLen, p, d) $
                                    go allPts ((l,lLen,segPLen/2):(h,hLen,segPLen/2):segs) p d
      in go
         pts'
         [(fullSeg, fullSegLen, 1)]
         0 0

    computePlacement CSP{cspPoint=fPt, cspTangent=fTan} CSP{cspPoint=rPt, cspTangent=rTan} =
      Placement fPt rPt $ signedAngleBetween fTan rTan




example :: _ => QDiagram _ V2 Double Any
example = curve
          # explodePath
          # mconcat
          # zipWith lc [uncurryRGB sRGB $ hsl h 1 0.6 | h <- [0,0.5..]]
          # fmap (lineCap LineCapRound)
          # mconcat
          # flip atop (square 2.5 # fc black)
  where
    f = circle 2
    r = circle 1.62 # translateY 0.488
    curve = spirograph f r origin 1020.96 0.1
