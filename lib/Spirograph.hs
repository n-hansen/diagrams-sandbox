{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-partial-type-signatures #-}
module Spirograph where

import           Data.Colour.RGBSpace (uncurryRGB)
import           Data.Colour.RGBSpace.HSL

-- spirograph fixed rolling = cubicSpline False points
--   where
--     stepSize = 0.01
--     totalLength = 1

--     points = go totalLength 0 (cycle . pathTrails $ fixed) 0 (cycle . pathTrails $ rolling)

--     go distanceLeft fixedOffset (fixedHead:fixedTail) rollingOffset (rollingHead:rollingTail) =
--       let fixedTraveled = fixedHead `atParam` fixedOffset
--           rollingTraveled = rollingHead `atParam` rollingOffset

--       if | distanceLeft <= 0 -> []
--          |


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

spirograph :: Located (Trail V2 Double) -> Located (Trail V2 Double) -> Path V2 Double
spirograph fixed rolling = cubicSpline False points
  where
    stepSize = 0.1
    totalLength = 30

    points = go 0

    go dist | dist > totalLength = []
            | otherwise = pointAt dist : go (dist + stepSize)

    pointAt dist =
      let fixedParam = stdArcLengthToParam fixed dist
          rollingParam = stdArcLengthToParam rolling dist
          fixedTangent = tangentAtParam fixed fixedParam
          rollingTangent = tangentAtParam rolling rollingParam
          rotationAmount = signedAngleBetween rollingTangent fixedTangent
          penLocation = rotateAround (rolling `atParam` rollingParam) rotationAmount origin
          penOffsetFromContactPoint = (rolling `atParam` rollingParam) .-. penLocation
      in (fixed `atParam` fixedParam) .+^ penOffsetFromContactPoint

example :: _ => QDiagram _ V2 Double Any
example = curve
          # explodePath
          # mconcat
          # zipWith lc [uncurryRGB sRGB $ hsl h 1 0.6 | h <- [0,1..]]
          # mconcat
  where
    f = circle 2
    r = circle 1.1 # translateY 0.288
    place x = origin `placeAt` computePlacement f 0 r 0 x
    curve = fromVertices . fmap place $ [0,0.1..70]
