module Example exposing (..)

-- import Fuzz exposing (Fuzzer, int, list, string)

import ConnectFourV3Kata1.Board as Board
import Expect exposing (Expectation)
import Set
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
            [ test "board width & height should be greater than 0" <|
                \_ ->
                    let
                        dim =
                            { width = 7, height = 6 }

                        board =
                            Board.init dim
                    in
                    [ 0, 1, 0, 1, 0, 1, 0, 1 ]
                        |> List.foldl Board.insert board
                        |> Board.info
                        |> .state
                        |> Expect.equal (Board.GameOver (Board.PlayerWon Board.P1 Set.empty))
            ]
        ]
