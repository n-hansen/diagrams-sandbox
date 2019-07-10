import Diagrams.Backend.Cairo

ell :: Diagram B
ell = fromOffsets [ unitY
                  , unitX
                  , 2 *^ unit_Y
                  , 2 *^ unit_X
                  , unitY
                  , unitX
                  ]
      # strokeLoop
      # centerXY

lv2 = mconcat [ ell
                # fc red
              , ell
                # rotateBy 0.75
                # translate (r2 (-1, -1))
                # fc purple
              , ell
                # translate (r2 (1,-1))
                # fc blue
              , ell
                # rotateBy 0.25
                # translate (r2 (1,1))
                # fc green
              ]

main = renderCairo out sz d
  where
    out = "out/lfrac.png"
    sz = mkSizeSpec2D (Just 400) Nothing
    d = lv2
