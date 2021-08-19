module ColourTests exposing (..)

import Test exposing (..)
import Expect
import Colour

example : (String -> String) -> String -> String -> Test
example fn input expected =
  test (String.concat [input, " = ", expected]) <|
    \_ ->
      fn input
        |> Expect.equal expected

suite : Test
suite =
  describe "Colour"
  [ describe "hexToRgbString"
    [ describe "hex values can be converted" <|
        [ example (Colour.hexToRgbString) "FFFFFF" "rgb(255, 255, 255)"
        , example (Colour.hexToRgbString) "EEEEEE" "rgb(238, 238, 238)"
        , example (Colour.hexToRgbString) "000000" "rgb(0, 0, 0)"
        ]
    , describe "lowercase hex values can be converted" <|
        [ example (Colour.hexToRgbString) "ffffff" "rgb(255, 255, 255)"
        , example (Colour.hexToRgbString) "eeeeee" "rgb(238, 238, 238)"
        , example (Colour.hexToRgbString) "000000" "rgb(0, 0, 0)"
        ]
    , describe "three character hex values can be converted" <|
        [ example (Colour.hexToRgbString) "eee" "rgb(238, 238, 238)"
        , example (Colour.hexToRgbString) "fff" "rgb(255, 255, 255)"
        , example (Colour.hexToRgbString) "EEE" "rgb(238, 238, 238)"
        , example (Colour.hexToRgbString) "FFF" "rgb(255, 255, 255)"
        ]
    , describe "hex values can have a hash prefix" <|
        [ example (Colour.hexToRgbString) "#ffffff" "rgb(255, 255, 255)"
        , example (Colour.hexToRgbString) "#eee" "rgb(238, 238, 238)"
        , example (Colour.hexToRgbString) "#000000" "rgb(0, 0, 0)"
        ]
    ]
  , describe "hexToHslString"
    [ describe "hex values can be converted" <|
        [ example (Colour.hexToHslString) "FFFFFF" "hsl(0, 0%, 100%)"
        , example (Colour.hexToHslString) "EEEEEE" "hsl(0, 0%, 93%)"
        , example (Colour.hexToHslString) "000000" "hsl(0, 0%, 0%)"
        , example (Colour.hexToHslString) "FFE100" "hsl(53, 100%, 50%)"
        , example (Colour.hexToHslString) "442ED1" "hsl(248, 64%, 50%)"
        ]
    , describe "lowercase hex values can be converted" <|
        [ example (Colour.hexToHslString) "ffffff" "hsl(0, 0%, 100%)"
        , example (Colour.hexToHslString) "eeeeee" "hsl(0, 0%, 93%)"
        , example (Colour.hexToHslString) "000000" "hsl(0, 0%, 0%)"
        ]
    , describe "three character hex values can be converted" <|
        [ example (Colour.hexToHslString) "eee" "hsl(0, 0%, 93%)"
        , example (Colour.hexToHslString) "fff" "hsl(0, 0%, 100%)"
        , example (Colour.hexToHslString) "EEE" "hsl(0, 0%, 93%)"
        , example (Colour.hexToHslString) "FFF" "hsl(0, 0%, 100%)"
        , example (Colour.hexToHslString) "20f" "hsl(248, 100%, 50%)"
        ]
    , describe "hex values can have a hash prefix" <|
        [ example (Colour.hexToHslString) "#ffffff" "hsl(0, 0%, 100%)"
        , example (Colour.hexToHslString) "#eee" "hsl(0, 0%, 93%)"
        , example (Colour.hexToHslString) "#000000" "hsl(0, 0%, 0%)"
        , example (Colour.hexToHslString) "#FFE100" "hsl(53, 100%, 50%)"
        ]
    ]
  ]
