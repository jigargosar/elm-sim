module MirrorPuzzleV3.Main exposing (main)

import Dict exposing (Dict)
import Dict2d
import Graph.Tree as Tree exposing (Tree)
import Html
import Html.Attributes
import MirrorPuzzleV3.PositionTree as PositionTree
import Number2 exposing (Int2)
import Playground.Direction8 as D exposing (Direction8)
import PointFree exposing (flip)


main =
    viewForest 0 lightForest


viewForest : Int -> List (Tree a) -> Html.Html msg
viewForest level forest =
    Html.div []
        (List.filterMap (Tree.root >> Maybe.map (viewTree level)) forest)


viewTree : Int -> ( a, List (Tree b) ) -> Html.Html msg
viewTree level ( label, forest ) =
    Html.div [ Html.Attributes.class "ml3 pl3 bl b--red" ]
        [ Html.div [ Html.Attributes.class "pv3  " ] [ Html.text (Debug.toString label) ]
        , viewForest (level + 1) forest
        ]


type El
    = Start (List Direction8)
    | Continue
    | Split (List Direction8)
    | End


grid : Dict Int2 El
grid =
    Dict2d.fromListsWithDefault Continue
        [ [ Continue, Continue, Continue, Split [ D.left ] ]
        , [ Start [ D.right ], Continue, Continue, Split [ D.down, D.up ] ]
        , [ Split [ D.right ], Continue, Continue, Split [ D.left ] ]
        ]
        |> Tuple.second


nextDirections : (Int2 -> Maybe El) -> Direction8 -> Int2 -> Maybe (List Direction8)
nextDirections elAt previousDirection position =
    case elAt position of
        Just el ->
            case el of
                Split directions ->
                    Just directions

                End ->
                    Nothing

                Start _ ->
                    Just [ previousDirection ]

                Continue ->
                    Just [ previousDirection ]

        Nothing ->
            Nothing


lightForest : List (Tree Int2)
lightForest =
    let
        toLightTree ( position, el ) =
            case el of
                Start dirs ->
                    PositionTree.unfold
                        { position = position
                        , directions = dirs
                        , getNextDirections =
                            nextDirections (flip Dict.get grid)
                        }
                        |> Just

                _ ->
                    Nothing
    in
    Dict.toList grid |> List.filterMap toLightTree
