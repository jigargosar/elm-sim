module MirrorPuzzleV3.Main exposing (main)

import Dict2d
import Graph.Tree exposing (unfoldTree)
import Html
import Playground.Direction8 as D exposing (Direction8)


main =
    let
        next : number -> ( number, List number )
        next seed =
            ( seed
            , if seed == 1 then
                [ 2, 3, 4 ]

              else
                []
            )

        tree2 =
            unfoldTree next 1
    in
    tree2
        |> Debug.toString
        |> Html.text


type El
    = Start (List Direction8)
    | Continue
    | Split (List Direction8)
    | End


grid =
    Dict2d.fromListsWithDefault Continue
        [ [ Start [ D.fromInt 0 ]
          ]
        ]
