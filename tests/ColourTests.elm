module ColourTests exposing (..)

import Test exposing (..)
import Expect
import Colour

example : String -> String -> Test
example input expected =
  test (String.concat [input, " = ", expected]) <|
    \_ ->
      Colour.hexToRgb input
        |> Expect.equal expected

suite : Test
suite =
  describe "Colour"
  [ describe "hexToRgb"
    [ describe "hex values can be converted" <|
        [ example "FFFFFF" "rgb(255, 255, 255)"
        , example "EEEEEE" "rgb(238, 238, 238)"
        , example "000000" "rgb(0, 0, 0)"
        ]
    , describe "lowercase hex values can be converted" <|
        [ example "ffffff" "rgb(255, 255, 255)"
        , example "eeeeee" "rgb(238, 238, 238)"
        , example "000000" "rgb(0, 0, 0)"
        ]
    , describe "three character hex values can be converted" <|
        [ example "eee" "rgb(238, 238, 238)"
        , example "fff" "rgb(255, 255, 255)"
        , example "EEE" "rgb(238, 238, 238)"
        , example "FFF" "rgb(255, 255, 255)"
        ]
    , describe "hex values can have a hash prefix" <|
        [ example "#ffffff" "rgb(255, 255, 255)"
        , example "#eee" "rgb(238, 238, 238)"
        , example "#000000" "rgb(0, 0, 0)"
        ]
    ]
  ]
