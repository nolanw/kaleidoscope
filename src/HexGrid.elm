module HexGrid exposing (
  Hex,
  hexToScreenCorners,
  Layout,
  mapShapeHex,
  Point,
  supplyGrid)

type alias Point = { x: Float, y: Float }

type alias Hex = { q: Int, r: Int }
type alias FractionalHex = { q: Float, r: Float }
type alias CubeHex = { q: Int, r: Int, s: Int }
type alias FractionalCubeHex = { q: Float, r: Float, s: Float }

type alias Layout = { size: Float }

hexToScreenCenter : Layout -> Hex -> Point
hexToScreenCenter {size} {q,r} =
  let
    f0 = 3/2
    f1 = 0
    f2 = (sqrt 3)/2
    f3 = sqrt 3
    q_f = toFloat q
    r_f = toFloat r
  in
    { x = ((f0 * q_f) + (f1 * r_f)) * size
    , y = ((f2 * q_f) + (f3 * r_f)) * size
    }

hexToScreenCorners : Layout -> Hex -> List Point
hexToScreenCorners layout hex =
  let
    center = hexToScreenCenter layout hex
    angleRad i = (degrees 60) * (toFloat i)
    hexCorner i =
      { x = center.x + (layout.size * cos(angleRad i))
      , y = center.y + (layout.size * sin(angleRad i))
      }
  in
    List.map hexCorner (List.range 0 5)

screenToHex : Layout -> Point -> Hex
screenToHex {size} {x,y} =
  let
    b0 = 2/3
    b1 = 0
    b2 = -1/3
    b3 = (sqrt 3)/3
    p =
      { x = x / size
      , y = y / size
      }
    frac =
      { q = (b0 * p.x) + (b1 * p.y)
      , r = (b2 * p.x) + (b3 * p.y)
      }
  in
    roundHex frac

roundHex : FractionalHex -> Hex
roundHex frac =
  let
    cubeFrac = axialToCube frac
    rounded =
      { q = toFloat (round cubeFrac.q)
      , r = toFloat (round cubeFrac.r)
      , s = toFloat (round cubeFrac.s)
      }
    qDiff = abs (rounded.q - cubeFrac.q)
    rDiff = abs (rounded.r - cubeFrac.r)
    sDiff = abs (rounded.s - cubeFrac.s)
    cubic =
      if qDiff > rDiff && qDiff > sDiff then
        { rounded | q = -rounded.r - rounded.s }
      else if rDiff > sDiff then
        { rounded | r = -rounded.q - rounded.s }
      else
        { rounded | s = rounded.q - rounded.r }
  in
    cubeToAxial (
      { q = round cubic.q
      , r = round cubic.r
      , s = round cubic.s
      }
    )

axialToCube : { q: number, r: number } -> { q: number, r: number, s: number }
axialToCube {q,r} =
  { q = q
  , r = r
  , s = -q - r
  }

cubeToAxial : { q: number, r: number, s: number } -> {q: number, r: number }
cubeToAxial {q,r} =
  { q = q
  , r = r
  }

mapShapeHex : Int -> List Hex
mapShapeHex radius =
  let
    qs = List.range -radius radius
    rs q = List.range
      (max -radius (-q - radius))
      (min radius (-q + radius))
    hex q r = { q = q, r = r }
  in
    List.concat (List.map (\q -> List.map (\r -> hex q r) (rs q)) qs)

supplyGrid : List Hex
supplyGrid =
  (moveAll (moveHorizontal -1) (mapShapeVerticalLine 9)) ++
  (mapShapeVerticalLine 9) ++
  (moveAll (moveHorizontal 1) (mapShapeVerticalLine 9))

mapShapeVerticalLine : Int -> List Hex
mapShapeVerticalLine height =
  let
    fromZero = List.range 0 (height - 1)
    offset = (height + 1) // 2
  in
    List.map (\r -> { q = 0, r = r - offset }) fromZero

moveHorizontal : Int -> Hex -> Hex
moveHorizontal offset {q, r} =
  { q = q + (2 * offset)
  , r = r - offset
  }

moveAll : (Hex -> Hex) -> List Hex -> List Hex
moveAll moveOne hexes =
  List.map moveOne hexes
