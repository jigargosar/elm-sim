module MirrorPuzzleV3.Main exposing (main)

import Color
import Dict exposing (Dict)
import Dict2d
import Graph.Tree as Tree exposing (Tree)
import Html
import Html.Attributes
import MirrorPuzzleV3.PositionTree as PositionTree
import Number2 as NT exposing (Int2)
import Playground.Direction8 as D exposing (Direction8)
import PointFree exposing (flip, mapEach)
import TypedSvg exposing (g, rect, svg)
import TypedSvg.Attributes exposing (fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx as PX
import TypedSvg.Types exposing (Fill(..), Transform(..))


main =
    Html.div [] [ gridView, viewForest 0 lightForest ]


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


cellSize =
    30


gridView =
    let
        ( w, h ) =
            gridDimensionsF |> NT.scale cellSize
    in
    svg [ viewBox 0 0 w h, PX.width w, PX.height h ]
        [ g
            [ transform [ Translate (w / 2) (h / 2), Scale 0.9 0.9, Translate (-w / 2) (-h / 2) ]
            ]
            (Dict.toList grid |> List.map (Tuple.mapFirst NT.toFloat >> viewGridItem))
        ]


viewGridItem ( position, el ) =
    let
        ( x, y ) =
            position |> NT.scale cellSize
    in
    rect
        [ transform [ Translate x y ]
        , PX.width cellSize
        , PX.height cellSize
        , stroke Color.black
        , fill FillNone
        ]
        []


type El
    = Start (List Direction8)
    | Continue
    | Split (List Direction8)
    | End


grid : Dict Int2 El
grid =
    gr |> Tuple.second


gr =
    Dict2d.fromListsWithDefault Continue
        [ [ Continue, Continue, Continue, Split [ D.left ] ]
        , [ Start [ D.right ], Continue, Continue, Split [ D.down, D.up ] ]
        , [ Split [ D.right ], Continue, Continue, Split [ D.left ] ]
        ]


gridDimensions =
    gr |> Tuple.first


gridDimensionsF =
    gridDimensions |> mapEach toFloat


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
