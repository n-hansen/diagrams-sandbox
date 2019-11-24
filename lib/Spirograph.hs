{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-partial-type-signatures -fno-warn-missing-signatures #-}
module Spirograph
  ( spirograph
  , spirographPoints
  , example
  ) where

import           Control.Arrow
import           Data.Colour.RGBSpace (uncurryRGB)
import           Data.Colour.RGBSpace.HSL


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

placeAt :: _ => _ -> Placement -> _
placeAt thing Placement{fixedContactPoint, rollingContactPoint, rollingRotation} =
  thing
  # translate (origin .-. rollingContactPoint)
  # rotate rollingRotation
  # translate (fixedContactPoint .-. origin)

spirograph f r p d s = fromVertices $ spirographPoints f r p d s

data TrailTraversal = TT { currSegment :: Located (Segment Closed V2 Double)
                         , currSegmentIx :: Int
                         , currDistLeft :: Double
                         , nextSegments :: [(Int,Located (Segment Closed V2 Double), Double)]
                         }

spirographPoints :: Located (Trail V2 Double) -> Located (Trail V2 Double) -> P2 Double -> Double -> Double -> [P2 Double]
spirographPoints fixedCurve rollingCurve penLocation distance stepSize = placeAt penLocation <$> placements
  where
    placements = fmap computePlacement
                 . uncurry zip
                 . (computeSegmentPoints *** computeSegmentPoints)
                 . unzip
                 . fmap (\(ptId,fSeg,fSegIx,fDistLeft,rSeg,rSegIx,rDistLeft) -> ( (fSegIx, ptId, fSeg, fDistLeft)
                                                                                , (rSegIx, ptId, rSeg, rDistLeft)
                                                                                )
                        )
                 $ samplePoints

    samplePoints :: [(Double, Located (Segment Closed V2 Double), Int, Double, Located (Segment Closed V2 Double), Int, Double)]
    samplePoints = buildSamplePoints 0 distance (initTrailTraversal fixedCurve) (initTrailTraversal rollingCurve)

    advanceAndBuildSamplePoints toGo fixedTraversal rollingTraversal =
      let advancement = min toGo $ minimum [currDistLeft fixedTraversal, currDistLeft rollingTraversal, stepSize]
      in buildSamplePoints advancement (toGo - advancement) fixedTraversal rollingTraversal

    buildSamplePoints advancement toGo fixedTraversal rollingTraversal =
      let (fSeg, fSegIx, fDistLeft, fTrav) = stepTrailTraversalBy fixedTraversal advancement
          (rSeg, rSegIx, rDistLeft, rTrav) = stepTrailTraversalBy rollingTraversal advancement
      in (toGo, fSeg, fSegIx, fDistLeft, rSeg, rSegIx, rDistLeft) : if toGo <= 0 then [] else advanceAndBuildSamplePoints toGo fTrav rTrav

    initTrailTraversal curve = let (initIx,initSeg,initDist):segs = cycle
                                                                    . fmap (\(ix,seg) -> (ix,seg,stdArcLength seg))
                                                                    . zip [0..]
                                                                    . trailLocSegments
                                                                    $ curve
                               in TT initSeg initIx initDist segs

    stepTrailTraversalBy tt@TT{currSegment,currSegmentIx,currDistLeft,nextSegments} delta =
      let currDistLeft' = currDistLeft - delta
          (nextSegmentIx, nextSegment, nextDistLeft):segs = nextSegments
          nextDistLeft' = nextDistLeft + currDistLeft'
      in if delta < currDistLeft
         then (currSegment, currSegmentIx, currDistLeft', tt {currDistLeft = currDistLeft'})
         else (nextSegment, nextSegmentIx, nextDistLeft', TT nextSegment nextSegmentIx nextDistLeft' segs)

    computeSegmentPoints = sortOn (\(ptId,_,_) -> -ptId)
                           . concatMap computeSingleSegmentPoints
                           . groupBy (on (==) $ \(ix,_,_,_) -> ix)
                           . sortOn (\(ix,_,_,d) -> (ix,-d))

    computeSingleSegmentPoints [] = []
    computeSingleSegmentPoints pts'@((_, _, fullSeg,_):_) =
      let fullSegLen = stdArcLength fullSeg
          go [] _ _ _ = []
          go allPts@((ptId,pt):pts) allSegs@((seg,segLen,segPLen):segs) p d =
            let entryAtParam atP = (ptId, seg `atParam` atP, seg `tangentAtParam` atP)
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

    computePlacement ((_,fPt,fTan),(_,rPt,rTan)) = Placement fPt rPt $ signedAngleBetween fTan rTan




example :: _ => QDiagram _ V2 Double Any
example = curve
          # explodePath
          # mconcat
          # zipWith lc [uncurryRGB sRGB $ hsl h 1 0.6 | h <- [0,1..]]
          # mconcat
  where
    f = circle 2
    r = circle 1.54 # translateY 0.488
    curve = spirograph f r origin 100 0.1
