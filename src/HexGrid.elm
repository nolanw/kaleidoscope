-- https://www.redblobgames.com/grids/hexagons/ and https://www.redblobgames.com/grids/hexagons/implementation.html

module HexGrid exposing (
  Hex,
  HexCorner(..),
  hexCornerToScreen,
  hexCornersToScreen,
  HexEdgeMidpoint(..),
  hexEdgeMidpointToScreen,
  Layout,
  mapShapeHex,
  mapShapeVerticalLine,
  moveHorizontal,
  Point)

type alias Point = { x: Float, y: Float }

type alias Hex = { q: Int, r: Int }

type alias Layout = { size: Float }

hexCenterToScreen : Layout -> Hex -> Point
hexCenterToScreen {size} {q,r} =
  let
    q_f = toFloat q
    r_f = toFloat r
  in
    { x = 3/2 * q_f * size
    , y = (((sqrt 3)/2 * q_f) + ((sqrt 3) * r_f)) * size
    }

type HexCorner
  = Right
  | BottomRight
  | BottomLeft
  | Left
  | TopLeft
  | TopRight

hexCornersToScreen : Layout -> Hex -> List Point
hexCornersToScreen layout hex =
  List.map (hexCornerToScreen layout hex)
    [ Right
    , BottomRight
    , BottomLeft
    , Left
    , TopLeft
    , TopRight
    ]

hexCornerToScreen : Layout -> Hex -> HexCorner -> Point
hexCornerToScreen layout hex point =
  let
    angle = degrees 60 * case point of
      Right -> 0
      BottomRight -> 1
      BottomLeft -> 2
      Left -> 3
      TopLeft -> 4
      TopRight -> 5
    center = hexCenterToScreen layout hex
  in
    { x = center.x + (layout.size * (cos angle))
    , y = center.y + (layout.size * (sin angle))
    }

type HexEdgeMidpoint
  = Southeast
  | South
  | Southwest
  | Northwest
  | North
  | Northeast

hexEdgeMidpointToScreen : Layout -> Hex -> HexEdgeMidpoint -> Point
hexEdgeMidpointToScreen layout hex midpoint =
  let
    points = List.map (hexCornerToScreen layout hex)
      <| case midpoint of
        Southeast -> [ Right, BottomRight ]
        South -> [ BottomRight, BottomLeft ]
        Southwest -> [ BottomLeft, Left ]
        Northwest -> [ Left, TopLeft ]
        North -> [ TopLeft, TopRight ]
        Northeast -> [ TopRight, Right ]
  in
    case points of
      [ a, b ] ->
        { x = (a.x + b.x) / 2
        , y = (a.y + b.y) / 2
        }
      
      _ -> { x = 0, y = 0 }

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
  in
    roundHex <|
      { q = (b0 * p.x) + (b1 * p.y)
      , r = (b2 * p.x) + (b3 * p.y)
      }
roundHex : { q: Float, r: Float } -> Hex
roundHex frac =
  let
    cubeFrac = axialToCube frac
    rounded =
      { q = round cubeFrac.q |> toFloat
      , r = round cubeFrac.r |> toFloat
      , s = round cubeFrac.s |> toFloat
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
cubeToAxial {q,r} = { q = q, r = r }

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

mapShapeVerticalLine : Int -> List Hex
mapShapeVerticalLine height =
  let
    offset = (height + 1) // 2
    hexify r = { q = 0, r = r - offset }
  in
    List.range 0 (height - 1)
      |> List.map hexify

moveHorizontal : Int -> Hex -> Hex
moveHorizontal offset {q,r} =
  { q = q + (2 * offset)
  , r = r - offset
  }
