module Example exposing (..)

import ConnectFourV2.Main as C4
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)



{-

   suite : Test
   suite =
       describe "The String module"
           [ describe "String.reverse"
               -- Nest as many descriptions as you like.
               [ test "has no effect on a palindrome" <|
                   \_ ->
                       let
                           palindrome =
                               "hannnah"
                       in
                       Expect.equal palindrome (String.reverse palindrome)

               -- Expect.equal is designed to be used in pipeline style, like this.
               , test "reverses a known string" <|
                   \_ ->
                       "ABCDEFG"
                           |> String.reverse
                           |> Expect.equal "GFEDCBA"

               -- fuzz runs the test 100 times with randomly-generated inputs!
               , fuzz string "restores the original string if you run it again" <|
                   \randomlyGeneratedString ->
                       randomlyGeneratedString
                           |> String.reverse
                           |> String.reverse
                           |> Expect.equal randomlyGeneratedString
               ]
           ]

-}


suite2 : Test
suite2 =
    describe "ValidBoard"
        [ describe "init"
            [ test "zero sized board is valid as long as there are no moves" <|
                \_ ->
                    C4.initBoard 0 0 []
                        |> Expect.notEqual Nothing
            ]
        , test "zero sized board are invalid with any move" <|
            \_ ->
                C4.initBoard 0 0 [ 0 ]
                    |> Expect.equal Nothing
        ]
