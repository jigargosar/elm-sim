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
    describe "Board State"
        [ test "horizontal win" <|
            \_ ->
                let
                    winningPositions =
                        List.range 0 3 |> List.map (\y -> ( 0, y )) |> Set.fromList
                in
                [ 0, 1, 0, 1, 0, 1, 0, 1 ]
                    |> boardStateAfterMoves
                    |> Expect.equal (p1Won winningPositions)
        ]


boardStateAfterMoves moves =
    moves
        |> List.foldl Board.insert (Board.init { width = 7, height = 6 })
        |> Board.info
        |> .state


p1Won wp =
    Board.GameOver (Board.PlayerWon Board.P1 wp)
