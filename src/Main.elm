port module Main exposing (..)

import Browser
import Dict
import HexGrid exposing (..)
import Html exposing ( Html, button, div )
import Html.Attributes
import Html.Events
import Svg exposing (..)
import Svg.Attributes as A exposing (..)
import Svg.Events exposing (..)
import Tile exposing (..)

-- Main

main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = updateWithStorage
    , subscriptions = subscriptions
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

type alias Flags = { boardEncoded: Maybe String }

init : Flags -> ( Model, Cmd Msg )
init flags =
  ( (modelInit flags)
  , Cmd.none
  )

modelInit : Flags -> Model
modelInit flags =
  let
    boardTiles = decodeBoardTiles flags.boardEncoded
  in
    { layout = { size = 30 }
    , selection = NoSelection
    , board = makeBoard <| if (List.isEmpty boardTiles) then (List.repeat 19 Nothing) else boardTiles
    , supply = makeSupply <| List.filterMap identity <| boardTiles
    }

decodeBoardTiles : Maybe String -> List (Maybe Tile)
decodeBoardTiles encoded =
  let
    decode s = s |> String.toList |> List.map tileDecode
    decodedBoardTiles_ = Maybe.map decode encoded
    decodedBoardTiles = Maybe.withDefault [] decodedBoardTiles_
  in
    if (List.length decodedBoardTiles) == 19 then
      decodedBoardTiles
    else
      []

makeBoard : List (Maybe Tile) -> List BoardHex
makeBoard tiles =
  let
    boardify tile hex = { tile = tile, hex = hex }
  in
    List.map2 boardify tiles (mapShapeHex 2)

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

supplyGrid : List Hex
supplyGrid =
  List.concat
    [ mapShapeVerticalLine 9 |> List.map (moveHorizontal -1)
    , mapShapeVerticalLine 9
    , mapShapeVerticalLine 9 |> List.map (moveHorizontal 1)
    ]

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
  | DeserializeBoard String

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
    
    DeserializeBoard encoded ->
      ( modelInit { boardEncoded = Just encoded }
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
        svg {
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
      ( [glow] ++ (List.map (boardPolygon layout selection) board) )
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
        , A.style "margin-left: 1em"
        ] ++ (List.filterMap (\m -> m)
        [ case selection of
          Board _ -> Just (Svg.Events.onClick RemoveFromBoard)
          Supply _ -> Nothing
          NoSelection -> Nothing
        , case selection of
          Board _ -> Just (class "hover-grey")
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
  Svg.filter [ id "glow" ]
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
        [ type_ "linear"
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
    fillColor = case selected of
      Selected -> "yellow"
      Deselected -> "white"
    hoverMaybe = case hover of
      CanHover -> Just (class "hover-grey")
      NoHover -> Nothing
    lines t =
      [ t |> tileLeft |> lineEdgeToEdge layout hex Southwest Northeast
      , t |> tileMiddle |> lineEdgeToEdge layout hex North South
      , t |> tileRight |> lineEdgeToEdge layout hex Southeast Northwest
      ]
    titles t =
      [ t |> tileLeft |> tileText layout hex LeftText
      , t |> tileMiddle |> tileText layout hex TopText
      , t |> tileRight |> tileText layout hex RightText
      ]
  in
    g []
      (List.concat
        [ [ polygon
            (
              [ points (hexCornersToScreen layout hex |> svgPoints)
              , fill fillColor
              , stroke "black"
              ] ++ List.filterMap (\m -> m)
              [ Maybe.map Svg.Events.onClick msg
              , hoverMaybe
              ]
            )
            []
          ]
        , Maybe.map lines tile |> Maybe.withDefault []
        , Maybe.map titles tile |> Maybe.withDefault []
        ]
      )

type Selected
  = Selected
  | Deselected

type TileTextPosition
  = LeftText
  | TopText
  | RightText

tileText : Layout -> Hex -> TileTextPosition -> Int -> Svg msg
tileText layout hex textPosition num =
  let
    anchor = case textPosition of
      LeftText -> hexEdgeMidpointToScreen layout hex Southwest
      TopText -> hexEdgeMidpointToScreen layout hex North
      RightText -> hexEdgeMidpointToScreen layout hex Southeast
    vertical = case textPosition of
      LeftText -> "bottom"
      TopText -> "hanging"
      RightText -> "bottom"
    horizontal = case textPosition of
      LeftText -> "start"
      TopText -> "middle"
      RightText -> "end"
  in
    text_
      [ x (anchor.x |> String.fromFloat)
      , y (anchor.y |> String.fromFloat)
      , A.filter "url(#glow)"
      , alignmentBaseline vertical
      , textAnchor horizontal
      ]
      [ num |> String.fromInt |> text ]

lineEdgeToEdge : Layout -> Hex -> HexEdgeMidpoint -> HexEdgeMidpoint -> Int -> Svg msg
lineEdgeToEdge layout hex startPoint endPoint num =
  let
    start = hexEdgeMidpointToScreen layout hex startPoint
    end = hexEdgeMidpointToScreen layout hex endPoint
  in
    line
      [ x1 (start.x |> String.fromFloat)
      , y1 (start.y |> String.fromFloat)
      , x2 (end.x |> String.fromFloat)
      , y2 (end.y |> String.fromFloat)
      , class ("line-" ++ (String.fromInt num))
      , strokeWidth (layout.size / 4 |> String.fromFloat)
      ]
      []

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
  List.map svgPoint points
    |> String.join " "

svgPoint : Point -> String
svgPoint p =
  let
    x = p.x |> String.fromFloat
    y = p.y |> String.fromFloat
  in
    x ++ "," ++ y

-- Ports

port setHash : String -> Cmd msg
port hashChanged : (String -> msg) -> Sub msg

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ =
  hashChanged DeserializeBoard

-- Storage

updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
  let
    ( newModel, cmds ) = update msg oldModel
    boardTiles = List.map .tile newModel.board
    encoded = List.map tileEncode boardTiles
    hash = String.fromList encoded
  in
    ( newModel
    , Cmd.batch [ setHash hash, cmds ]
    )
