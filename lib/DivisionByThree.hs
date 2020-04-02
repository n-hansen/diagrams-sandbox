{-# LANGUAGE PartialTypeSignatures #-}
module DivisionByThree where

import Data.List (zip3)

type String = [Char]

s  = square 2 # showOrigin # lw thick
ds = (s # named "1") ||| strutX 3 ||| (s # named "2")
t  = cubicSpline False (map p2 [(0, 0), (1, 0), (1, 0.2), (2, 0.2)])

example = ds # connect' (with & arrowHead .~ dart & lengths .~ veryLarge
                              & arrowTail .~ dart'
                              & shaftStyle %~ lw thick & arrowShaft .~ t) "1" "2"


grid :: _ => _ -> [String] -> [String] -> Diagram _
grid c rows cols = rows
                   # map makeRow
                   # foldr (===) mempty
                   # addColHeadings
  where
    makeRow rowName = cols
                      # map (makeCell rowName)
                      # foldr (|||) mempty
                      # (heading rowName |||)
    makeCell rowName colName = square 1
                               # fc c
                               # lw 2
                               # named (rowName <> "," <> colName)
    heading :: String -> _
    heading txt = text txt
                  # fontSizeL 0.7
                  -- # fc white
                  <>
                  square 1
                  # lw 0
    addColHeadings = ([""] <> cols)
                     # map heading
                     # foldr (|||) mempty
                     # (===)


mappingGrids = bothGrids # addConnections
  where
    bothGrids = grid red ["a","b","c"] ["0","1"]
                |||
                square 4 # lw 0
                |||
                grid blue ["x","y","z"] ["0","1"]
    addConnections = cxn shaftUp "a,0" "x,0"
                     . cxn shaftUp "a,1" "y,1"
                     . cxn shaftDown "b,0" "x,1"
                     . cxn shaftUp "b,1" "z,1"
                     . cxn shaftDown "c,0" "y,0"
                     . cxn shaftDown "c,1" "z,0"

    cxn s = connect' (with
                     & arrowHead .~ noHead
                     & lengths .~ small
                     & arrowTail .~ noHead
                     & shaftStyle %~ lw thick . lc green & arrowShaft .~ s
                   )

    shaftUp = arc xDir (-1/6 @@ turn)
    shaftDown = arc xDir (1/6 @@ turn)

arrowGrid :: _ => _ -> [String] -> [String] -> Diagram _
arrowGrid c rows cols = rows
                   # map makeRow
                   # foldr (===) mempty
                   # addColHeadings
  where
    makeRow rowName = cols
                      # map (makeCell rowName)
                      # foldr (|||) mempty
                      # atop bigArrow
                      # (heading rowName |||)
    makeCell rowName colName = square cellScale
                               -- # fc c
                               # lw 0
                               # named (rowName <> "," <> colName)
    firstCol = unsafeHead cols
    lastCol = unsafeLast cols
    cellScale = 1.2
    arrowScale = 30
    bigArrow = cols
               # map (\c -> square cellScale # lw 0 # named ("arr-" <> c))
               # foldr (|||) mempty
               # connect' (with
                           & shaftStyle %~ lw arrowScale . lc c
                           & headStyle %~ lw arrowScale . lc c . fc c
                           & arrowHead .~ tri
                           & tailGap .~ (-9)
                          )
               ("arr-" <> firstCol) ("arr-" <> lastCol)
    heading :: String -> _
    heading txt = text txt
                  # fontSizeL 0.7
                  -- # fc white
                  <>
                  square 1
                  # lw 0
    addColHeadings = ([""] <> cols)
                     # map heading
                     # foldr (|||) mempty
                     # (===)

arrowMappings = bothGrids # addConnections
  where
    bothGrids = arrowGrid red ["a","b","c"] ["0","1"]
                |||
                square 4 # lw 0
                |||
                arrowGrid blue ["x","y","z"] ["0","1"]
    addConnections = cxn shaftUp "a,0" "x,0"
                     . cxn shaftUp "a,1" "y,1"
                     . cxn shaftDown "b,0" "x,1"
                     . cxn shaftUp "b,1" "z,1"
                     . cxn shaftDown "c,0" "y,0"
                     . cxn shaftDown "c,1" "z,0"

    cxn s = connect' (with
                     & arrowHead .~ noHead
                     & lengths .~ small
                     & arrowTail .~ noHead
                     & shaftStyle %~ lw thick . lc green & arrowShaft .~ s
                   )

    shaftUp = arc xDir (-1/6 @@ turn)
    shaftDown = arc xDir (1/6 @@ turn)


hexLoop = labels `atop` arrows
  where
    hex = regPoly 6 2
    node id = circle 0.1
              # lw 0
              # named (show id :: String)
    cxn c f t = connect' ( with
                           & tailGap .~ 8
                           & headLength .~ large
                           & headGap .~ (-35)
                           & shaftStyle %~ lw thick . lc c
                           & headStyle %~ lc c . fc c -- . scale 9
                           & arrowHead .~ quill
                           -- & arrowHead .~ tri
                         ) (show f :: String) (show t :: String)
    arrows = atPoints (trailVertices hex) (map node [1..6])
             # cxn blue 2 1
             # cxn red 3 2
             # cxn blue 3 4
             # cxn red 5 4
             # cxn blue 5 6
             # cxn red 6 1
    lbl c l = text l # fontSizeL 0.7 # fc c
    labels = atPoints (trailVertices $ hex # scale 1.2 # rotate (1/12 @@ turn))
             [ lbl blue "z"
             , lbl red "c"
             , lbl blue "y"
             , lbl red "a"
             , lbl blue "x"
             , lbl red "b"
             ]

hexLoop' = labels `atop` arrows
  where
    hex = regPoly 6 2
    node id = circle 0.1
              # lw 0
              # named (show id :: String)
    cxn c f t = connect' ( with
                           & gaps .~ 8
                           & shaftStyle %~ lw thick . lc c
                           & headStyle %~ lc c . fc c
                           & arrowHead .~ tri
                         ) (show f :: String) (show t :: String)
    arrows = atPoints (trailVertices hex) (map node [1..6])
             # cxn blue 1 2
             # cxn red 2 3
             # cxn blue 3 4
             # cxn red 4 5
             # cxn blue 5 6
             # cxn red 6 1
    lbl c l = text l # fontSizeL 0.7 # fc c
    labels = atPoints (trailVertices $ hex # scale 1.2 # rotate (1/12 @@ turn))
             [ lbl blue "z"
             , lbl red "c"
             , lbl blue "y"
             , lbl red "a"
             , lbl blue "x"
             , lbl red "b"
             ]

chain = nodes
        # map (\n -> square 1 # lw 0 # named (show n :: String))
        # intersperse (square 3 # lw 0)
        # foldr (|||) mempty
        # addArrows
  where
    addArrows base = foldr
                     (\(f,t,c) -> connect' ( with
                                             & tailGap .~ 13
                                             & shaftStyle %~ lw thick . lc c
                                             & headStyle %~ lc c . fc c
                                             & headLength .~ large
                                             & headGap .~ (-17)
                                             & arrowHead .~ quill
                                           ) (show f :: String) (show t :: String)
                     ) base
                     arrows
    nodes = "abcdefg"
    arrows = zip3
             "acdeef"
             "bbcdfg"
             (cycle [red,blue])


renderMe = --chain
           --hexLoop
           arrowMappings
           -- arrowGrid red ["a","b","c"] ["1","2"]
           -- mappingGrids
           # pad 1.2
