module Colour exposing (hexToRgb)
import Regex
import ParseInt

hexRegex : Regex.Regex
hexRegex =
  Maybe.withDefault Regex.never <|
    Regex.fromStringWith
      { caseInsensitive = True, multiline = False }
      "^#?([a-f\\d]{2})([a-f\\d]{2})([a-f\\d]{2})$"

removeHashPrefix : String -> String
removeHashPrefix str =
  String.replace "#" "" str

-- TODO: should this have validation on string length?
-- TODO: should this be better lol
shortHexToFullHex : String -> String
shortHexToFullHex value =
  case String.length value of
    3 ->
      (String.slice 0 1 value) ++
      (String.slice 0 1 value) ++
      (String.slice 1 2 value) ++
      (String.slice 1 2 value) ++
      (String.slice 2 3 value) ++
      (String.slice 2 3 value)
    _ -> value

parseHexString : String -> Int
parseHexString hexVal =
  case ParseInt.parseIntHex hexVal of
    Ok int -> int
    _ -> 0

-- TODO: fix parseHex behaviour - just putting bad stuff here rn
parseHex : String -> List Int
parseHex hex =
  case List.head (Regex.find hexRegex hex) of
    Just match ->
      case .submatches match of
        Just(a) :: Just(b) :: Just(c) :: _ -> List.map parseHexString [a, b, c]
        _ -> [0, 0, 0]
    Nothing -> [0, 0, 0]

hexToRgb : String -> String
hexToRgb hex =
  -- hex
  -- |> removeHashPrefix
  -- |> shortHexToFullHex
  String.concat
  [ "rgb("
  , String.join ", " (List.map String.fromInt (parseHex (shortHexToFullHex (removeHashPrefix hex))))
  , ")"
  ]
  -- case hex of
  --   "FFFFFF" -> "rgb(255, 255, 255)"
  --   "ffffff" -> "rgb(255, 255, 255)"
  --   "eeeeee" -> "rgb(238, 238, 238)"
  --   "EEE" -> "rgb(238, 238, 238)"
  --   "FFF" -> "rgb(255, 255, 255)"
  --   "fff" -> "rgb(255, 255, 255)"
  --   "eee" -> "rgb(238, 238, 238)"
  --   _ -> "rgb(0, 0, 0)"
