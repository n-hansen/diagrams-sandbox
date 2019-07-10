import Diagrams.Backend.Cairo.CmdLine

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
      # fc white

main = mainWith
       ell
