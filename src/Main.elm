port module Main exposing (..)

import Browser
import Dict
import HexGrid exposing (..)
import Html exposing (Html, button, div)
import Html.Attributes
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)

-- Main

main : Program E.Value Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = updateWithStorage
    , subscriptions = \_ -> Sub.none
    }

-- Model

type alias Model =
  { layout: Layout
  , board: List BoardHex
  , supply: List SupplyHex
  , selection: Selection
  }

type Selection
  = Supply Tile
  | Board Tile
  | NoSelection

init : E.Value -> ( Model, Cmd Msg )
init flags =
  let
    layout = { size = 30 }
    selection = NoSelection
  in
    (
      case D.decodeValue decoder flags of
        Ok boardTiles ->
          { layout = layout
          , selection = selection
          , board = makeBoard boardTiles
          , supply = makeSupply (List.filterMap .tile boardTiles)
          }
          
        Err msg ->
          { layout = layout
          , selection = selection
          , board = makeBoard []
          , supply = makeSupply []
          }
    ,
      Cmd.none
    )

makeBoard : List BoardHex -> List BoardHex
makeBoard boardTiles =
  let
    toTuple bt = Maybe.map
      (\t -> ((bt.hex.q, bt.hex.r), t))
      bt.tile
    lookup = boardTiles
      |> List.filterMap toTuple
      |> Dict.fromList
  in
    mapShapeHex 2
      |> List.map (\h ->
        { hex = h
        , tile = Dict.get (h.q, h.r) lookup
        })

makeSupply : List Tile -> List SupplyHex
makeSupply boardTiles =
  let
    tileState tile =
      if List.any (\t -> t == tile) boardTiles then
        Was tile
      else
        Now tile
    combine hex tile = { hex = hex, tile = tileState tile }
  in
    List.map2 combine supplyGrid supplyTiles

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

type Msg
  = SelectFromSupply Tile
  | PlaceOnBoard Hex
  | SelectOnBoard Tile
  | RemoveFromBoard
  | Deselect
  | ClearBoard

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SelectFromSupply tile ->
      ( updateSupplySelection model tile
      , Cmd.none
      )

    PlaceOnBoard hex ->
      ( updatePlaceOnBoard model hex
      , Cmd.none
      )
    
    SelectOnBoard tile ->
      ( updateBoardSelection model tile
      , Cmd.none
      )
    
    RemoveFromBoard ->
      ( updateRemoveFromBoard model
      , Cmd.none
      )
    
    Deselect ->
      ( { model | selection = NoSelection }
      , Cmd.none
      )
    
    ClearBoard ->
      let
        erase bh = { bh | tile = Nothing }
        replace sh = case sh.tile of
          Now _ -> sh
          Was t -> { sh | tile = Now t }
      in
      ( { model
        | board = List.map erase model.board
        , supply = List.map replace model.supply
        }
      , Cmd.none
      )
      
updateSupplySelection : Model -> Tile -> Model
updateSupplySelection model target =
  let
    matches sh = case sh of
      Now tile -> tile == target
      Was _ -> False
    available = List.any matches (List.map .tile model.supply)
  in
    { model | selection = if available then Supply target else NoSelection }

updatePlaceOnBoard : Model -> Hex -> Model
updatePlaceOnBoard model hex =
  case model.selection of
    NoSelection ->
      model
    
    Board tile ->
      let
        boardUpdate bh =
          case bh.tile of
            Just t ->
              if t == tile then
                { bh | tile = Nothing }
              else
                bh
            
            Nothing ->
              if bh.hex == hex then
                { bh | tile = Just tile }
              else
                bh
      in
        { model
        | board = List.map boardUpdate model.board
        , selection = NoSelection
        }

    Supply tile ->
      let
        boardUpdate bh =
          case bh.tile of
            Just _ ->
              bh
            
            Nothing ->
              if bh.hex == hex then
                { bh | tile = Just tile }
              else
                bh
        supplyUpdate sh =
          case sh.tile of
            Now t -> if t == tile then { sh | tile = Was t } else sh
            Was _ -> sh
      in
        { model
        | board = List.map boardUpdate model.board
        , supply = List.map supplyUpdate model.supply
        , selection = NoSelection
        }

updateBoardSelection : Model -> Tile -> Model
updateBoardSelection model tile =
  let
    matches bh = case bh.tile of
      Just t -> t == tile
      Nothing -> False
    available = List.any matches model.board
  in
    { model | selection = if available then Board tile else NoSelection }

updateRemoveFromBoard : Model -> Model
updateRemoveFromBoard model =
  let
    boardReplace tile bh = case bh.tile of
      Just t -> if t == tile then { bh | tile = Nothing } else bh
      Nothing -> bh
    supplyReplace tile sh = case sh.tile of
      Was t -> if t == tile then { sh | tile = Now t } else sh
      Now _ -> sh
  in case model.selection of
    Board tile ->
      { model
      | board = List.map (boardReplace tile) model.board
      , supply = List.map (supplyReplace tile) model.supply
      , selection = NoSelection
      }
    Supply _ -> model
    NoSelection -> model


-- View

view : Model -> Html Msg
view {layout, board, supply, selection} =
  div
    [ Html.Attributes.style "display" "flex"
    , Html.Attributes.style "align-items" "start"
    ]
    [ Html.node "style" [] [ Html.text
        """
        body {
          font-family: system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", "Roboto", "Oxygen", "Ubuntu", "Cantarell", "Fira Sans", "Droid Sans", "Helvetica Neue", sans-serif;
        }

        .hover-grey:hover { background: grey }

        svg text {
          cursor: default;
          -webkit-user-select: none;
          -moz-user-select: none;
          -ms-user-select: none;
          user-select: none;
        }

        svg line {
          pointer-events: none
        }

        line.line-1 { stroke: #aaa }
        line.line-2 { stroke: #ffccee }
        line.line-3 { stroke: #ff2f92 }
        line.line-4 { stroke: #34abff }
        line.line-5 { stroke: #00bdc0 }
        line.line-6 { stroke: #ff2600 }
        line.line-7 { stroke: #cdf275 }
        line.line-8 { stroke: #ff9300 }
        line.line-9 { stroke: #fff700 }
        """
    ]
    , svg
      (let
        w = findWidth layout (List.map .hex board)
        h = 2 + findHeight layout (List.map .hex board)
        x = -w/2
        y = -h/2 - 1
      in
        [ width (String.fromFloat w)
        , height (String.fromFloat h)
        , viewBox (String.join " " (List.map String.fromFloat [x, y, w, h]))
        ]
      )
      ( [ Svg.style
          [ Svg.Attributes.type_ "text/css" ]
          [ Svg.text ".hover-grey:hover { fill: #ccc }" ]
        , glow
        ] ++ (List.map (boardPolygon layout selection) board)
      )
    , button
      [ Html.Events.onClick ClearBoard ]
      [ Html.text "Clear Board" ]
    , svg
      (let
        w = findWidth layout (List.map .hex supply)
        h = 2 + findHeight layout (List.map .hex supply)
        x = -w/2
        y = -h/2 - 1
      in
        [ width (String.fromFloat w)
        , height (String.fromFloat h)
        , viewBox (String.join " " (List.map String.fromFloat [x, y, w, h]))
        , Svg.Attributes.style "margin-left: 1em"
        ] ++ (List.filterMap (\m -> m)
        [ case selection of
          Board _ -> Just (Svg.Events.onClick RemoveFromBoard)
          Supply _ -> Nothing
          NoSelection -> Nothing
        , case selection of
          Board _ -> Just (Svg.Attributes.class "hover-grey")
          Supply _ -> Nothing
          NoSelection -> Nothing
        ])
      )
      ([ glow ] ++
        (let
          tileHexify {hex, tile} =
            { hex = hex
            , tile = case tile of
              Now t -> Just t
              Was _ -> Nothing
            }
        in
          List.map (supplyPolygon layout selection) (List.map tileHexify supply)
        )
      )
    ]

glow : Svg msg
glow =
  -- https://stackoverflow.com/a/54115866
  Svg.filter [ Svg.Attributes.id "glow" ]
    [ feFlood
      [ floodColor "#fff"
      , floodOpacity "0.7"
      , in_ "SourceGraphic"
      ] []
    , feComposite
      [ operator "in"
      , in2 "SourceGraphic"
      ] []
    , feGaussianBlur [ stdDeviation "1" ] []
    , feComponentTransfer [ result "glow1" ]
      [ feFuncA
        [ Svg.Attributes.type_ "linear"
        , slope "10"
        , intercept "0"
        ] []
      ]
    , feMerge []
      [ feMergeNode [ in_ "glow1" ] []
      , feMergeNode [ in_ "SourceGraphic" ] []
      ]
    ]

boardPolygon : Layout -> Selection -> BoardHex -> Svg Msg
boardPolygon layout selection bh =
  let
    msg = case (bh.tile, selection) of
      (Nothing, Supply _) ->
        Just (PlaceOnBoard bh.hex)
      (Nothing, Board _) ->
        Just (PlaceOnBoard bh.hex)
      (Just bt, NoSelection) ->
        Just (SelectOnBoard bt)
      (Just bt, Board t) ->
        Just (if bt == t then Deselect else SelectOnBoard t)
      (_, _) ->
        Nothing
    hover = case (bh.tile, selection) of
      (Nothing, Supply _) -> CanHover
      (_, _) -> NoHover
    selected = case (bh.tile, selection) of
      (Just bt, Board t) -> if bt == t then Selected else Deselected
      (_, _) -> Deselected
  in
    makePolygon layout selected hover msg bh

supplyPolygon : Layout -> Selection -> BoardHex -> Svg Msg
supplyPolygon layout selection th =
  let
    msg = case (th.tile, selection) of
      (Just t, Supply tile) ->
        Just (if t == tile then Deselect else SelectFromSupply t)
      
      (Just t, Board _) ->
        Just (SelectFromSupply t)
      
      (Just t, NoSelection) ->
        Just (SelectFromSupply t)
      
      (_, _) ->
        Nothing
    
    selected = case (selection, th.tile) of
      (Supply t, Just tile) -> if t == tile then Selected else Deselected
      (_, _) -> Deselected
  in
    makePolygon layout selected NoHover msg th

findWidth : Layout -> List Hex -> Float
findWidth {size} hexes =
  let
    qs = List.map .q hexes
    minQ = List.minimum qs
    maxQ = List.maximum qs
  in
    case (minQ, maxQ) of
       (Just min, Just max) -> size * 2 * (toFloat (max - min))
       (_, _) -> 0

findHeight : Layout -> List Hex -> Float
findHeight {size} hexes =
  let
    rs = List.map .r hexes
    minR = List.minimum rs
    maxR = List.maximum rs
  in
    case (minR, maxR) of
      (Just min, Just max) -> size * (sqrt 3) * (toFloat (max - min + 1))
      (_, _) -> 0

type Hover
  = CanHover
  | NoHover

makePolygon : Layout -> Selected -> Hover -> Maybe Msg -> BoardHex -> Svg Msg
makePolygon layout selected hover msg {hex, tile} =
  let
    fill = Svg.Attributes.fill <| case selected of
      Selected -> "yellow"
      Deselected -> "white"
    hoverMaybe = case hover of
      CanHover -> Just (class "hover-grey")
      NoHover -> Nothing
    lines t =
      [ t |> tileLeft |> lineLeft layout hex
      , t |> tileMiddle |> lineMiddle layout hex
      , t |> tileRight |> lineRight layout hex
      ]
    titles t =
      [ t |> tileLeft |> tileTextLeft layout hex
      , t |> tileMiddle |> tileTextTop layout hex
      , t |> tileRight |> tileTextRight layout hex
      ]
  in
    [ polygon
      (
        [ hexToScreenCorners layout hex |> svgPoints |> points
        , fill
        , stroke "black"
        ] ++ List.filterMap (\m -> m)
        [ Maybe.map Svg.Events.onClick msg
        , hoverMaybe
        ]
      )
      []
    ] ++ (Maybe.withDefault [] (Maybe.map lines tile))
      ++ (Maybe.withDefault [] (Maybe.map titles tile))
      |> g []

type Selected
  = Selected
  | Deselected

tileTextLeft : Layout -> Hex -> Int -> Svg msg
tileTextLeft layout hex num =
  let
    corners = hexToScreenCorners layout hex
    xs = corners |> List.map .x |> List.sort
    left = xs |> List.take 2 |> average
    ys = corners |> List.map .y |> List.sort
    bottom = ys |> List.drop 3 |> List.take 2 |> average
  in
    text_
      [ x (String.fromFloat left)
      , y (String.fromFloat bottom)
      , Svg.Attributes.filter "url(#glow)"
      ]
      [ num |> String.fromInt |> Svg.text ]

tileTextTop : Layout -> Hex -> Int -> Svg msg
tileTextTop layout hex num =
  let
    corners = hexToScreenCorners layout hex
    xs = corners |> List.map .x |> List.sort
    left = xs |> List.drop 2 |> List.take 2 |> average
    ys = corners |> List.map .y |> List.sort
    bottom = ys |> List.drop 1 |> List.take 2 |> average
  in
    text_
        [ x (String.fromFloat left)
        , y (String.fromFloat bottom)
        , Svg.Attributes.filter "url(#glow)"
        , textAnchor "middle"
        ]
        [ num |> String.fromInt |> Svg.text
        ]

tileTextRight : Layout -> Hex -> Int -> Svg msg
tileTextRight layout hex num =
  let
    corners = hexToScreenCorners layout hex
    xs = corners |> List.map .x |> List.sort
    left = xs |> List.drop 4 |> List.take 2 |> average
    ys = corners |> List.map .y |> List.sort
    bottom = ys |> List.drop 3 |> List.take 2 |> average
  in
    text_
        [ x (String.fromFloat left)
        , y (String.fromFloat bottom)
        , Svg.Attributes.filter "url(#glow)"
        , textAnchor "end"
        ]
        [ num |> String.fromInt |> Svg.text
        ]

lineLeft : Layout -> Hex -> Int -> Svg msg
lineLeft layout hex num =
  let
    corners = hexToScreenCorners layout hex
    xs = corners |> List.map .x |> List.sort
    left = xs |> List.take 2 |> average
    right = xs |> List.drop 4 |> List.take 2 |> average
    ys = corners |> List.map .y |> List.sort
    bottom = ys |> List.drop 3 |> List.take 2 |> average
    top = ys |> List.drop 1 |> List.take 2 |> average
  in
    line
      [ x1 <| String.fromFloat left
      , y1 <| String.fromFloat bottom
      , x2 <| String.fromFloat right
      , y2 <| String.fromFloat top
      , Svg.Attributes.class <| "line-" ++ (String.fromInt num)
      , strokeWidth <| String.fromFloat (layout.size / 4)
      ]
      []

lineMiddle : Layout -> Hex -> Int -> Svg msg
lineMiddle layout hex num =
  let
    corners = hexToScreenCorners layout hex
    xs = corners |> List.map .x |> List.sort
    x = xs |> List.drop 2 |> List.take 2 |> average
    ys = corners |> List.map .y |> List.sort
    top = ys |> List.head |> Maybe.withDefault 0
    bottom = ys |> List.drop 5 |> List.head |> Maybe.withDefault 0
  in
    line
      [ x1 <| String.fromFloat x
      , y1 <| String.fromFloat bottom
      , x2 <| String.fromFloat x
      , y2 <| String.fromFloat top
      , Svg.Attributes.class <| "line-" ++ (String.fromInt num)
      , strokeWidth <| String.fromFloat (layout.size / 4)
      ]
      []

lineRight : Layout -> Hex -> Int -> Svg msg
lineRight layout hex num =
  let
    corners = hexToScreenCorners layout hex
    xs = corners |> List.map .x |> List.sort
    left = xs |> List.take 2 |> average
    right = xs |> List.drop 4 |> List.take 2 |> average
    ys = corners |> List.map .y |> List.sort
    top = ys |> List.drop 1 |> List.take 2 |> average
    bottom = ys |> List.drop 3 |> List.take 2 |> average
  in
    line
      [ x1 <| String.fromFloat left
      , y1 <| String.fromFloat top
      , x2 <| String.fromFloat right
      , y2 <| String.fromFloat bottom
      , Svg.Attributes.class <| "line-" ++ (String.fromInt num)
      , strokeWidth <| String.fromFloat (layout.size / 4)
      ]
      []

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

type alias BoardHex =
  { hex: Hex
  , tile: Maybe Tile
  }

type alias SupplyHex =
  { hex: Hex
  , tile: SupplyTile
  }

type alias TileHex =
  { hex: Hex
  , tile: Maybe Tile
  }

type SupplyTile
  = Now Tile
  | Was Tile

svgPoints : List Point -> String
svgPoints points =
  String.join " " (List.map (\p -> String.fromFloat(p.x) ++ "," ++ String.fromFloat(p.y)) points)


-- Ports

port setStorage : E.Value -> Cmd msg

updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
  let
    ( newModel, cmds ) = update msg oldModel
  in
    ( newModel
    , Cmd.batch [ setStorage (encode newModel), cmds ]
    )


-- JSON encode/decode

encode : Model -> E.Value
encode model =
  E.object
    [ ("board", E.list (\m -> m) (List.filterMap encodeBoardHex model.board) ) ]

encodeBoardHex : BoardHex -> Maybe E.Value
encodeBoardHex bh =
  case bh.tile of
    Nothing -> Nothing
    Just t -> Just (E.object
      [ ("hex", encodeHex bh.hex)
      , ("tile", encodeTile t)
      ])

encodeHex : Hex -> E.Value
encodeHex {q, r} =
  [q, r]
    |> List.map String.fromInt
    |> String.join ","
    |> E.string

encodeTile : Tile -> E.Value
encodeTile tile =
  [ tileLeft, tileMiddle, tileRight ]
    |> List.map (\f -> f tile)
    |> List.map String.fromInt
    |> String.join ""
    |> E.string

decoder : D.Decoder (List BoardHex)
decoder =
  D.field "board" (D.list decodeBoardHex)

decodeBoardHex : D.Decoder BoardHex
decodeBoardHex =
  D.map2 BoardHex
    (D.field "hex" D.string
      |> D.andThen decodeHexString)
    (D.field "tile" D.string
      |> D.andThen decodeTileString)

decodeHexString : String -> D.Decoder Hex
decodeHexString hex =
  let
    components = String.split "," hex
    ints = List.map String.toInt components
  in case ints of
    [Just q, Just r] -> D.succeed { q = q, r = r }
    _ -> D.fail <|
      "Trying to decode hex coordinate, but "
      ++ hex ++ " is unrecognizable."

decodeTileString : String -> D.Decoder (Maybe Tile)
decodeTileString tile =
  case String.toList tile of
    [l, m, r] ->
      case (tileLeftFromChar l, tileMiddleFromChar m, tileRightFromChar r) of
        (Just left, Just middle, Just right) ->
          D.succeed (Just ( left, middle, right ))
        
        _ ->
          D.fail <|
            "Trying to decode tile, but "
            ++ tile ++ " is unrecognizable."
    _ ->
      D.fail <|
        "Trying to decode tile, but "
        ++ tile ++ " is not recognized."

tileLeftFromChar : Char -> Maybe TileLeft
tileLeftFromChar c =
  case c of
    '2' -> Just Two
    '6' -> Just Six
    '7' -> Just Seven
    _ -> Nothing

tileMiddleFromChar : Char -> Maybe TileMiddle
tileMiddleFromChar c =
  case c of
    '1' -> Just One
    '5' -> Just Five
    '9' -> Just Nine
    _ -> Nothing

tileRightFromChar : Char -> Maybe TileRight
tileRightFromChar c =
  case c of
    '3' -> Just Three
    '4' -> Just Four
    '8' -> Just Eight
    _ -> Nothing
