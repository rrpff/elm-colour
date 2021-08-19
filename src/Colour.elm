module Colour exposing (hexToRgbString, hexToHslString)
import Regex
import ParseInt

type alias Hex = String
type alias Rgb = { r : Float, g : Float, b : Float }
type alias Hsl = { h : Float, s : Float, l : Float }

hexRegex : Regex.Regex
hexRegex =
  Maybe.withDefault Regex.never <|
    Regex.fromStringWith
      { caseInsensitive = True, multiline = False }
      "^#?([a-f\\d]{2})([a-f\\d]{2})([a-f\\d]{2})$"

removeHashPrefix : Hex -> Hex
removeHashPrefix str =
  String.replace "#" "" str

-- TODO: should this have validation on string length?
-- TODO: should this be better lol
shortHexToFullHex : Hex -> Hex
shortHexToFullHex value =
  case String.length value of
    3 -> List.foldl (\v s -> s ++ v ++ v) "" (String.split "" value)
    _ -> value

parseIntHex : Hex -> Float
parseIntHex hexVal =
  case ParseInt.parseIntHex hexVal of
    Ok int -> toFloat int
    _ -> 0

-- TODO: fix parseHexAsRgb behaviour - just putting bad stuff here rn
parseHexAsRgb : Hex -> Rgb
parseHexAsRgb hex =
  case List.head (Regex.find hexRegex hex) of
    Just match ->
      case .submatches match of
        Just(r) :: Just(g) :: Just(b) :: _ ->
          { r = parseIntHex(r), g = parseIntHex(g), b = parseIntHex(b) }
        _ ->
          { r = 0, g = 0, b = 0 }
    Nothing ->
      { r = 0, g = 0, b = 0 }

normaliseHue : Float -> Float
normaliseHue hue =
  if hue < 0 then
    toFloat (round hue + 360)
  else
    toFloat (round hue)

-- TODO: do better
rgbToHsl : Rgb -> Hsl
rgbToHsl rgb =
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
    { h = normaliseHue hue
    , s = toFloat (round (saturation * 100))
    , l = toFloat (round (lightness * 100))
    }

rgbToString : Rgb -> String
rgbToString rgb =
  "rgb(" ++
  (String.fromFloat rgb.r) ++
  ", " ++
  (String.fromFloat rgb.g) ++
  ", " ++
  (String.fromFloat rgb.b) ++
  ")"

hslToString : Hsl -> String
hslToString hsl =
  "hsl(" ++
  (String.fromFloat hsl.h) ++
  ", " ++
  (String.fromFloat hsl.s) ++
  "%, " ++
  (String.fromFloat hsl.l) ++
  "%)"

hexToRgbString : Hex -> String
hexToRgbString hex =
  hex
  |> removeHashPrefix
  |> shortHexToFullHex
  |> parseHexAsRgb
  |> rgbToString

hexToHslString : Hex -> String
hexToHslString hex =
  hex
  |> removeHashPrefix
  |> shortHexToFullHex
  |> parseHexAsRgb
  |> rgbToHsl
  |> hslToString
