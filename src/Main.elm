port module Main exposing (..)

import Browser
import HexGrid exposing (..)
import Html exposing (Html, button, div)
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
  , hexes: List Hex
  }

init : () -> ( Model, Cmd Msg )
init flags =
  ( { layout = { size = 30 }
    , hexes = mapShapeHex 2
    }
  , Cmd.none
  )

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
view {layout, hexes} =
  div []
    [ svg
      (let
        w = findWidth layout hexes
        h = findHeight layout hexes
        x = -w/2
        y = -h/2
      in
        [ width (String.fromFloat w)
        , height (String.fromFloat h)
        , viewBox (String.join " " (List.map String.fromFloat [x, y, w, h]))
        ]
      )
      (List.map (makePolygon layout) hexes)
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

makePolygon : Layout -> Hex -> Svg Msg
makePolygon layout hex =
  polygon
    [ points (svgPoints (hexToScreenCorners layout hex))
    , fill "white"
    , stroke "black"
    , Svg.Events.onClick (Alert hex)
    ]
    []

svgPoints : List Point -> String
svgPoints points =
  String.join " " (List.map (\p -> String.fromFloat(p.x) ++ "," ++ String.fromFloat(p.y)) points)
