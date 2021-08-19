module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Url.Parser exposing ((</>), (<?>), int)
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

type Route
  = HslRoute Int Int Int

routeParser : Url.Parser.Parser (Route -> a) a
routeParser =
  Url.Parser.oneOf
    [ Url.Parser.map HslRoute (Url.Parser.s "hsl" </> int </> int </> int)
    ]

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
  case Url.Parser.parse routeParser url of
    (Just (HslRoute h s l)) ->
      ((Model key url (toFloat h) (toFloat s) (toFloat l) (Just "#FFF")) |> setBackgroundColour, Cmd.none)
    _ ->
      ((Model key url 0 0 100 (Just "#FFF")) |> setBackgroundColour, Cmd.none)

setBackgroundColour : Model -> Model
setBackgroundColour model =
  { model | background = Colour.toString(Just (Hsl { h = model.hue, s = model.saturation, l = model.lightness })) }

formatCurrentRoute : Model -> String
formatCurrentRoute model =
  "/hsl/" ++ String.fromFloat model.hue ++ "/" ++ String.fromFloat model.saturation ++ "/" ++ String.fromFloat model.lightness

-- UPDATE

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | HueChanged String
  | SaturationChanged String
  | LightnessChanged String

replaceUrl : Model -> Cmd msg
replaceUrl model =
  Nav.replaceUrl model.key (formatCurrentRoute model)

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
      let
        next = setBackgroundColour { model | hue = Maybe.withDefault 0 (String.toFloat hue) }
      in
        (next, replaceUrl next)

    SaturationChanged saturation ->
      let
        next = setBackgroundColour { model | saturation = Maybe.withDefault 0 (String.toFloat saturation) }
      in
        (next, replaceUrl next)

    LightnessChanged lightness ->
      let
        next = setBackgroundColour { model | lightness = Maybe.withDefault 0 (String.toFloat lightness) }
      in
        (next, replaceUrl next)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "colours"
  , body =
    [ Html.node "style" [] [text ("body { margin: 0; height: 100vh; }")]
    , section
      [ style "background-color" (Maybe.withDefault "#FFF" model.background)
      , style "display" "flex"
      , style "align-items" "center"
      , style "justify-content" "center"
      , style "flex-direction" "column"
      , style "height" "100%"
      ]
      [ section
        [ style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "flex-direction" "column"
        , style "padding" "100px"
        ]
        [ span [] [text "hue"]
        , input [type_ "range", Html.Attributes.min "0", Html.Attributes.max "360", onInput HueChanged, value (String.fromFloat model.hue)] []
        , span [] [text "saturation"]
        , input [type_ "range", Html.Attributes.min "0", Html.Attributes.max "100", onInput SaturationChanged, value (String.fromFloat model.saturation)] []
        , span [] [text "lightness"]
        , input [type_ "range", Html.Attributes.min "0", Html.Attributes.max "100", onInput LightnessChanged, value (String.fromFloat model.lightness)] []
        ]
      , span [] [text (Maybe.withDefault "#FFF" model.background)]
      ]
    ]
  }
