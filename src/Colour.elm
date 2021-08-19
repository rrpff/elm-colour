module Colour exposing (Colour(..), toRgb, toHsl, toString)
import Regex
import ParseInt
import Dict exposing (remove)

type Colour
  = Hex String
  | Rgb { r : Float, g : Float, b : Float }
  | Hsl { h : Float, s : Float, l : Float }

hexRegex : Regex.Regex
hexRegex =
  Maybe.withDefault Regex.never <|
    Regex.fromStringWith
      { caseInsensitive = True, multiline = False }
      "^#?([a-f\\d]{2})([a-f\\d]{2})([a-f\\d]{2})$"

removeHashPrefix : String -> String
removeHashPrefix str =
  String.replace "#" "" str

shortHexToFullHex : String -> String
shortHexToFullHex value =
  case String.length value of
    3 -> List.foldl (\v s -> s ++ v ++ v) "" (String.split "" value)
    _ -> value

formatHex : String -> String
formatHex hex = hex |> removeHashPrefix |> shortHexToFullHex

parseIntHex : String -> Float
parseIntHex hexVal =
  case ParseInt.parseIntHex hexVal of
    Ok int -> toFloat int
    _ -> 0

parseHexAsRgb : String -> Colour
parseHexAsRgb hex =
  case List.head (Regex.find hexRegex hex) of
    Just match ->
      case .submatches match of
        Just(r) :: Just(g) :: Just(b) :: _ ->
          Rgb { r = parseIntHex(r), g = parseIntHex(g), b = parseIntHex(b) }
        _ ->
          Rgb { r = 0, g = 0, b = 0 }
    Nothing ->
      Rgb { r = 0, g = 0, b = 0 }

normaliseHue : Float -> Float
normaliseHue hue =
  if hue < 0 then
    toFloat (round hue + 360)
  else
    toFloat (round hue)

-- TODO: do better
rgbToHsl : Maybe Colour -> Maybe Colour
rgbToHsl val =
  case val of
    Just (Rgb rgb) ->
      let
        (r, g, b) = (rgb.r / 255, rgb.g / 255, rgb.b / 255)
        min = (Maybe.withDefault 0 (List.minimum [r, g, b]))
        max = (Maybe.withDefault 0 (List.maximum [r, g, b]))

        delta = max - min
        hueDecimal =
          if max == min then
            0
          else if max == r then
            (g - b) / delta
          else if max == g then
            2 + (b - r) / delta
          else
            4 + (r - g) / delta

        hue = (Maybe.withDefault 0 (List.minimum [hueDecimal * 60, 360]))

        lightness = (min + max) / 2

        saturation =
          if max == min then
            0
          else if lightness <= 0.5 then
            delta / (min + max)
          else
            delta / (2 - min - max)
      in
        Just (Hsl
          { h = normaliseHue hue
          , s = toFloat (round (saturation * 100))
          , l = toFloat (round (lightness * 100))
          }
        )
    _ -> Nothing

rgbToString : Colour -> Maybe String
rgbToString colour =
  case colour of
    Rgb rgb ->
      Just (
        "rgb(" ++
        (String.fromFloat rgb.r) ++
        ", " ++
        (String.fromFloat rgb.g) ++
        ", " ++
        (String.fromFloat rgb.b) ++
        ")"
      )
    _ -> Nothing

hslToString : Colour -> Maybe String
hslToString colour =
  case colour of
    Hsl hsl ->
      Just (
        "hsl(" ++
        (String.fromFloat hsl.h) ++
        ", " ++
        (String.fromFloat hsl.s) ++
        "%, " ++
        (String.fromFloat hsl.l) ++
        "%)"
      )
    _ -> Nothing

toRgb : Colour -> Maybe Colour
toRgb colour =
  case colour of
    Rgb rgb -> Just (Rgb rgb)
    Hex hex -> Just (hex |> formatHex |> parseHexAsRgb)
    _ -> Nothing

toHsl : Colour -> Maybe Colour
toHsl colour =
  case colour of
    Hsl hsl -> Just (Hsl hsl)
    Hex hex -> colour |> toRgb |> rgbToHsl
    _ -> Nothing

toString : Maybe Colour -> Maybe String
toString colour =
  case colour of
    Just (Hex c) -> Just c
    Just (Rgb c) -> rgbToString (Rgb c)
    Just (Hsl c) -> hslToString (Hsl c)
    _ -> Nothing
