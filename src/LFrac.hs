import           Diagrams.Backend.Cairo

ell :: Diagram B
ell =
  fromOffsets [ unitY
              , unitX
              , 2 *^ unit_Y
              , 2 *^ unit_X
              , unitY
              , unitX
              ]
  # strokeLoop
  # centerXY
  # fc red

layTiles tile =
  let tile' tr rot o =
        opacity o
        . translate tr
        . rotateBy rot
        . scale 0.5
        $ tile
      t1 = tile' zero 0 1
      t2 = tile' (r2 (negate xShift, negate yShift)) 0.75 0.8
      t3 = tile' (r2 (xShift, negate yShift)) 0 0.8
      t4 = tile' (r2 (xShift, yShift)) 0.25 0.8
  in
    mconcat [t1,t2,t3,t4]

  where
    xShift = 0.5
    yShift = 0.5

main = renderCairo out sz d
  where
    out = "out/lfrac.png"
    sz = mkSizeSpec2D (Just 400) Nothing
    d = layTiles . layTiles . layTiles $ ell
