port module Main exposing (..)

import Browser
import Html exposing (Html, button, div)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)

-- Main

main : Program () Model Msg
main =
  Browser.element
    { init = \_ -> ( 0, Cmd.none )
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

-- Ports

port alert : String -> Cmd msg

-- Model

type alias Model = Int

-- Update

type Msg = Increment | Decrement | Alert
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Increment ->
      ( model + 1
      , Cmd.none
      )
    
    Decrement ->
      ( model - 1
      , Cmd.none
      )
    
    Alert ->
      ( model
      , alert (String.fromInt model)
      )

-- View

view : Model -> Html Msg
view model =
  div []
    [ button [ Html.Events.onClick Decrement ] [ Html.text "-" ]
    , svg
      [ width "120"
      , height "120"
      , viewBox "0 0 120 120"
      ]
      [ text_
        [ x "0"
        , y "30"
        , Svg.Events.onClick Alert
        ]
        [ Svg.text (String.fromInt model) ]
      ]
    , button [ Html.Events.onClick Increment ] [ Html.text "+" ]
    ]
