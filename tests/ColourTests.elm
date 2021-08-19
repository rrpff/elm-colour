module ColourTests exposing (..)

import Test exposing (..)
import Expect
import Colour exposing (Colour(..), toRgb, toHsl, toString)

example : String -> String -> Maybe String -> Test
example input expected fn =
  case (fn) of
    Just result ->
      test (String.concat [input, " = ", expected]) <|
        \_ ->
          Expect.equal expected result
    _ ->
      test (String.concat [input, " = ", expected]) <|
        \_ ->
          Expect.fail "Result was Nothing"

suite : Test
suite =
  describe "Colour"
  [ describe "toRgb"
    [ describe "hex values can be converted" <|
        [ example "FFFFFF" "rgb(255, 255, 255)" ((Hex "FFFFFF") |> toRgb |> toString)
        , example "EEEEEE" "rgb(238, 238, 238)" ((Hex "EEEEEE") |> toRgb |> toString)
        , example "000000" "rgb(0, 0, 0)" ((Hex "000000") |> toRgb |> toString)
        ]
    , describe "lowercase hex values can be converted" <|
        [ example "ffffff" "rgb(255, 255, 255)" ((Hex "ffffff") |> toRgb |> toString)
        , example "eeeeee" "rgb(238, 238, 238)" ((Hex "eeeeee") |> toRgb |> toString)
        , example "000000" "rgb(0, 0, 0)" ((Hex "000000") |> toRgb |> toString)
        ]
    , describe "three character hex values can be converted" <|
        [ example "eee" "rgb(238, 238, 238)" ((Hex "eee") |> toRgb |> toString)
        , example "fff" "rgb(255, 255, 255)" ((Hex "fff") |> toRgb |> toString)
        , example "EEE" "rgb(238, 238, 238)" ((Hex "EEE") |> toRgb |> toString)
        , example "FFF" "rgb(255, 255, 255)" ((Hex "FFF") |> toRgb |> toString)
        ]
    , describe "hex values can have a hash prefix" <|
        [ example "#ffffff" "rgb(255, 255, 255)" ((Hex "#ffffff") |> toRgb |> toString)
        , example "#eee" "rgb(238, 238, 238)" ((Hex "#eee") |> toRgb |> toString)
        , example "#000000" "rgb(0, 0, 0)" ((Hex "#000000") |> toRgb |> toString)
        ]
    ]
  , describe "hexToHslString"
    [ describe "hex values can be converted" <|
        [ example "FFFFFF" "hsl(0, 0%, 100%)" (Hex("FFFFFF") |> toHsl |> toString)
        , example "EEEEEE" "hsl(0, 0%, 93%)" (Hex("EEEEEE") |> toHsl |> toString)
        , example "000000" "hsl(0, 0%, 0%)" (Hex("000000") |> toHsl |> toString)
        , example "FFE100" "hsl(53, 100%, 50%)" (Hex("FFE100") |> toHsl |> toString)
        , example "442ED1" "hsl(248, 64%, 50%)" (Hex("442ED1") |> toHsl |> toString)
        ]
    , describe "lowercase hex values can be converted" <|
        [ example "ffffff" "hsl(0, 0%, 100%)" (Hex("ffffff") |> toHsl |> toString)
        , example "eeeeee" "hsl(0, 0%, 93%)" (Hex("eeeeee") |> toHsl |> toString)
        , example "000000" "hsl(0, 0%, 0%)" (Hex("000000") |> toHsl |> toString)
        ]
    , describe "three character hex values can be converted" <|
        [ example "eee" "hsl(0, 0%, 93%)" (Hex("eee") |> toHsl |> toString)
        , example "fff" "hsl(0, 0%, 100%)" (Hex("fff") |> toHsl |> toString)
        , example "EEE" "hsl(0, 0%, 93%)" (Hex("EEE") |> toHsl |> toString)
        , example "FFF" "hsl(0, 0%, 100%)" (Hex("FFF") |> toHsl |> toString)
        , example "20f" "hsl(248, 100%, 50%)" (Hex("20f") |> toHsl |> toString)
        ]
    , describe "hex values can have a hash prefix" <|
        [ example "#ffffff" "hsl(0, 0%, 100%)" (Hex("#ffffff") |> toHsl |> toString)
        , example "#eee" "hsl(0, 0%, 93%)" (Hex("#eee") |> toHsl |> toString)
        , example "#000000" "hsl(0, 0%, 0%)" (Hex("#000000") |> toHsl |> toString)
        , example "#FFE100" "hsl(53, 100%, 50%)" (Hex("#FFE100") |> toHsl |> toString)
        ]
    ]
  ]
