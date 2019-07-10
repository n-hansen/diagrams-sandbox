import           Diagrams.Backend.Cairo
import           Random

ell = do
  c <- randomHue 1 1
  return $
    fromOffsets [ unitY
                , unitX
                , 2 *^ unit_Y
                , 2 *^ unit_X
                , unitY
                , unitX
                ]
    # strokeLoop
    # centerXY
    # fc c

layTiles tile =
  let tile' tr rot =
        translate tr
        . rotateBy rot
        . scale 0.5
        <$> tile
      t1 = tile' zero 0
      t2 = tile' (r2 (negate xShift, negate yShift)) 0.75
      t3 = tile' (r2 (xShift, negate yShift)) 0
      t4 = tile' (r2 (xShift, yShift)) 0.25
  in
    mconcat <$> sequence [t1,t2,t3,t4]

  where
    xShift = 0.5
    yShift = 0.5

main = renderCairo out sz =<< d
  where
    out = "out/lfrac.png"
    sz = mkSizeSpec2D (Just 400) Nothing
    d = runRandom [1] . layTiles . layTiles . layTiles . layTiles $ ell
