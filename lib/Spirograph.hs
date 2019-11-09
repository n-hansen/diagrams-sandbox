{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-partial-type-signatures #-}
module Spirograph where

import           Data.Colour.RGBSpace (uncurryRGB)
import           Data.Colour.RGBSpace.HSL

data Placement = Placement { fixedContactPoint :: P2 Double
                           , rollingContactPoint :: P2 Double
                           , rollingRotation :: Angle Double
                           } deriving Show

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

spirograph f r p d s = fromVertices $ spirograph' f r p d s

example :: _ => QDiagram _ V2 Double Any
example = curve
          # explodePath
          # mconcat
          # zipWith lc [uncurryRGB sRGB $ hsl h 1 0.6 | h <- [0,1..]]
          # mconcat
  where
    f = circle 2
    r = circle 1.1 # translateY 0.288
    curve = spirograph f r origin 70 0.1
