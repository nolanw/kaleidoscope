port module Main exposing (..)

import Browser
import HexGrid exposing (..)
import Html exposing (Html, button, div)
import Html.Attributes
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)

-- Main

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

-- Ports

port alert : String -> Cmd msg

-- Model

type alias Model =
  { layout: Layout
  , board: List TiledHex
  , supply: List TiledHex
  }

init : () -> ( Model, Cmd Msg )
init flags =
  ( { layout = { size = 30 }
    , board = (List.map (\h -> { hex = h, tile = Nothing }) (mapShapeHex 2))
    , supply = List.map2 (\h tile -> { hex = h, tile = Just tile }) supplyGrid supplyTiles
    }
  , Cmd.none
  )

supplyTiles : List Tile
supplyTiles =
  let
    left = [Two, Six, Seven]
    top = [One, Five, Nine]
    right = [Three, Four, Eight]
  in
    List.concatMap
      (\l -> List.concatMap
        (\t -> List.map
          (\r -> (l,t,r))
          right
        )
        top
      )
      left

-- Update

type Msg = Alert Hex
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Alert hex ->
      ( model
      , alert
        ( "clicked hex " ++ (String.fromInt hex.q)
        ++ "," ++ (String.fromInt hex.r)
        )
      )

-- View

view : Model -> Html Msg
view {layout, board, supply} =
  div
    [ Html.Attributes.style "display" "flex"
    , Html.Attributes.style "align-items" "start"
    ]
    [ svg
      (let
        w = findWidth layout (List.map .hex board)
        h = findHeight layout (List.map .hex board)
        x = -w/2
        y = -h/2
      in
        [ width (String.fromFloat w)
        , height (String.fromFloat h)
        , viewBox (String.join " " (List.map String.fromFloat [x, y, w, h]))
        ]
      )
      (List.map (makePolygon layout) board)
    , svg
      (let
        w = findWidth layout (List.map .hex supply)
        h = findHeight layout (List.map .hex supply)
        x = -w/2
        y = -h/2
      in
        [ width (String.fromFloat w)
        , height (String.fromFloat h)
        , viewBox (String.join " " (List.map String.fromFloat [x, y, w, h]))
        , Svg.Attributes.style "margin-left: 1em"
        ]
      )
      (List.map (makePolygon layout) supply)
    ]

findWidth : Layout -> List Hex -> Float
findWidth {size} hexes =
  let
    qs = List.map .q hexes
    minQ = List.minimum qs
    maxQ = List.maximum qs
  in
    case (minQ, maxQ) of
       (Nothing, _) -> 0
       (_, Nothing) -> 0
       (Just min, Just max) -> size * 2 * (toFloat (max - min))

findHeight : Layout -> List Hex -> Float
findHeight {size} hexes =
  let
    rs = List.map .r hexes
    minR = List.minimum rs
    maxR = List.maximum rs
  in
    case (minR, maxR) of
       (Nothing, _) -> 0
       (_, Nothing) -> 0
       (Just min, Just max) -> size * (sqrt 3) * (toFloat (max - min + 1))

makePolygon : Layout -> TiledHex -> Svg Msg
makePolygon layout {hex,tile} =
  g []
  ([ polygon
    [ points (svgPoints (hexToScreenCorners layout hex))
    , fill "white"
    , stroke "black"
    , Svg.Events.onClick (Alert hex)
    ]
    []
  ] ++ (List.filterMap (\m -> m)
    [ Maybe.map (\t -> t |> tileLeft |> tileTextLeft layout hex) tile
    , Maybe.map (\t -> t |> tileMiddle |> tileTextTop layout hex) tile
    , Maybe.map (\t -> t |> tileRight |> tileTextRight layout hex) tile
    ]
  )
  )

tileTextLeft : Layout -> Hex -> Int -> Svg msg
tileTextLeft layout hex num =
  let
    corners = hexToScreenCorners layout hex
    xs = corners |> List.map .x |> List.sort
    left = average (List.take 2 xs)
    ys = corners |> List.map .y |> List.sort
    bottom = average (List.take 2 (List.drop 3 ys))
  in
    text_
      [ x (String.fromFloat left)
      , y (String.fromFloat bottom)
      ]
      [ num |> String.fromInt |> Svg.text
      ]

tileTextTop : Layout -> Hex -> Int -> Svg msg
tileTextTop layout hex num =
  let
    corners = hexToScreenCorners layout hex
    xs = corners |> List.map .x |> List.sort
    left = average (List.take 2 (List.drop 2 xs))
    ys = corners |> List.map .y |> List.sort
    bottom = average (List.take 2 (List.drop 1 ys))
  in
    text_
        [ x (String.fromFloat left)
        , y (String.fromFloat bottom)
        ]
        [ num |> String.fromInt |> Svg.text
        ]

tileTextRight : Layout -> Hex -> Int -> Svg msg
tileTextRight layout hex num =
  let
    corners = hexToScreenCorners layout hex
    xs = corners |> List.map .x |> List.sort
    left = average (List.take 2 (List.drop 3 xs))
    ys = corners |> List.map .y |> List.sort
    bottom = average (List.take 2 (List.drop 3 ys))
  in
    text_
        [ x (String.fromFloat left)
        , y (String.fromFloat bottom)
        ]
        [ num |> String.fromInt |> Svg.text
        ]

average : List Float -> Float
average xs =
  case xs of
     [] -> 0
     _ -> (List.sum xs) / (xs |> List.length |> toFloat)

type alias Tile = ( TileLeft, TileMiddle, TileRight )
type TileLeft = Two | Six | Seven
type TileMiddle = One | Five | Nine
type TileRight = Three | Four | Eight

tileLeft : Tile -> Int
tileLeft tile =
  case tile of
    (Two, _, _) -> 2
    (Six, _, _) -> 6
    (Seven, _, _) -> 7

tileMiddle : Tile -> Int
tileMiddle tile =
  case tile of
    (_, One, _) -> 1
    (_, Five, _) -> 5
    (_, Nine, _) -> 9

tileRight : Tile -> Int
tileRight tile =
  case tile of
     (_, _, Three) -> 3
     (_, _, Four) -> 4
     (_, _, Eight) -> 8

type alias TiledHex =
  { hex: Hex
  , tile: Maybe Tile
  }

svgPoints : List Point -> String
svgPoints points =
  String.join " " (List.map (\p -> String.fromFloat(p.x) ++ "," ++ String.fromFloat(p.y)) points)
