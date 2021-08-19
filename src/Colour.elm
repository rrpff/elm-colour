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
    3 -> List.foldl (\v s -> s ++ v ++ v) "" (String.split "" value)
    _ -> value

parseHexString : String -> Int
parseHexString hexVal =
  case ParseInt.parseIntHex hexVal of
    Ok int -> int
    _ -> 0

-- TODO: fix parseHexAsRgb behaviour - just putting bad stuff here rn
parseHexAsRgb : String -> List Int
parseHexAsRgb hex =
  case List.head (Regex.find hexRegex hex) of
    Just match ->
      case .submatches match of
        Just(a) :: Just(b) :: Just(c) :: _ -> List.map parseHexString [a, b, c]
        _ -> [0, 0, 0]
    Nothing -> [0, 0, 0]

hexToRgb : String -> String
hexToRgb hex =
  String.concat
  [ "rgb("
  , String.join ", " (List.map String.fromInt (hex |> removeHashPrefix |> shortHexToFullHex |> parseHexAsRgb))
  , ")"
  ]
