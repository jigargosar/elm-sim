module MirrorPuzzleV3.Main exposing (main)

import Dict exposing (Dict)
import Dict2d
import Graph.Tree exposing (unfoldTree)
import Html
import Number2 exposing (Int2)
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


grid : ( Int2, Dict Int2 El )
grid =
    Dict2d.fromListsWithDefault Continue
        [ [ Continue, Continue, Continue, Split [ D.left ] ]
        , [ Start [ D.right ], Continue, Continue, Split [ D.down, D.up ] ]
        , [ Continue, Continue, Continue, Split [ D.left ] ]
        ]
