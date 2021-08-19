module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Colour exposing (Colour(..))
import Html.Events exposing (onInput)

-- MAIN

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }

-- MODEL

type alias Model =
  { key: Nav.Key
  , url: Url.Url
  , hue: Float
  , saturation: Float
  , lightness: Float
  , background: Maybe String
  }

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
  ((Model key url 60 90 70 (Just "#FFF")) |> setBackgroundColour, Cmd.none)

setBackgroundColour : Model -> Model
setBackgroundColour model =
  { model | background = Colour.toString(Just (Hsl { h = model.hue, s = model.saturation, l = model.lightness })) }

-- UPDATE

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | HueChanged String
  | SaturationChanged String
  | LightnessChanged String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UrlChanged url ->
      ({ model | url = url }, Cmd.none)

    LinkClicked request ->
      case request of
        Browser.Internal url ->
          (model, Nav.pushUrl model.key (Url.toString url))
        Browser.External url ->
          (model, Nav.load url)

    HueChanged hue ->
      ({ model | hue = Maybe.withDefault 0 (String.toFloat hue) } |> setBackgroundColour, Cmd.none)

    SaturationChanged saturation ->
      ({ model | saturation = Maybe.withDefault 0 (String.toFloat saturation) } |> setBackgroundColour, Cmd.none)

    LightnessChanged lightness ->
      ({ model | lightness = Maybe.withDefault 0 (String.toFloat lightness) } |> setBackgroundColour, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "colours"
  , body =
    [ section
      [ style "background-color" (Maybe.withDefault "#FFF" model.background)
      , style "width" "600px"
      , style "height" "600px"
      , style "max-width" "100%"
      , style "max-height" "100%"
      , style "display" "flex"
      , style "align-items" "center"
      , style "justify-content" "center"
      , style "flex-direction" "column"
      , style "margin" "auto"
      ]
      [ span [] [text "hue"]
      , input [type_ "range", Html.Attributes.min "0", Html.Attributes.max "360", onInput HueChanged, value (String.fromFloat model.hue)] []
      , span [] [text "saturation"]
      , input [type_ "range", Html.Attributes.min "0", Html.Attributes.max "100", onInput SaturationChanged, value (String.fromFloat model.saturation)] []
      , span [] [text "lightness"]
      , input [type_ "range", Html.Attributes.min "0", Html.Attributes.max "100", onInput LightnessChanged, value (String.fromFloat model.lightness)] []
      ]
    ]
  }

viewLink : String -> Html msg
viewLink path =
  li [] [a [href path] [text path]]
