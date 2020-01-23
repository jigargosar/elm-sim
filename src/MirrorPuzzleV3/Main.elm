module MirrorPuzzleV3.Main exposing (main)

import Basics.Extra exposing (swap, uncurry)
import Color
import Dict exposing (Dict)
import Dict2d
import Graph.Tree as Tree exposing (Forest, Tree)
import Html exposing (Html)
import Html.Attributes exposing (class)
import MirrorPuzzleV3.PositionTree as PositionTree
import Number2 as NT exposing (Int2)
import Playground.Direction8 as D exposing (Direction8)
import PointFree exposing (flip, mapEach)
import Set exposing (Set)
import Svg exposing (Svg, g)
import Svg.Attributes
import TypedSvg exposing (circle, line, rect, svg)
import TypedSvg.Attributes exposing (fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx as PX
import TypedSvg.Types exposing (Fill(..), Transform(..))


main =
    Html.div []
        [ gridView
        , viewForest 0 lightForest
        ]


viewLightForestEdgesEndpoints : EdgesEndPoints -> List (Svg msg)
viewLightForestEdgesEndpoints ( dict, eps ) =
    List.map (uncurry viewLine) (Dict.toList dict)
        ++ List.map viewEndPoint (Set.toList eps)


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
    100


gridView : Html.Html msg
gridView =
    Html.div [ class "pa2 inline-flex flex-wrap" ]
        [ Html.div [ class "inline-flex flex-column" ]
            [ Html.div [ class "tc pa2" ] [ Html.text "tree : view  recursion" ]
            , gridCanvasWith (viewLightForest lightForest)
            ]
        , Html.div [ class "inline-flex flex-column" ]
            [ Html.div [ class "tc pa2" ] [ Html.text "tree to graph" ]
            , gridCanvasWith
                (viewLightForestEdgesEndpoints (lightForestEdgesEndpoints lightForest))
            ]
        , Html.div [ class "inline-flex flex-column" ]
            [ Html.div [ class "tc pa2" ] [ Html.text "grid to graph" ]
            , gridCanvasWith
                (viewLightPathGraphs (lightPathGraphs grid))
            ]
        ]


viewLightPathGraphs : List (Graph Int2) -> List (Svg msg)
viewLightPathGraphs =
    List.concatMap viewLightPathGraph


viewLightPathGraph : Graph Int2 -> List (Svg msg)
viewLightPathGraph ( edges, eps ) =
    List.map (uncurry viewLine) (Set.toList edges)
        ++ List.map viewEndPoint (Set.toList eps)


gridCanvasWith : List (Svg msg) -> Html msg
gridCanvasWith children =
    let
        viewGridCells =
            Dict.toList grid |> List.map viewGridItem
    in
    Html.div [ class "pa2 flex" ] [ canvas (viewGridCells ++ children) ]


canvas : List (Svg msg) -> Html msg
canvas =
    let
        ( w, h ) =
            gridDimensionsF |> NT.scale cellSize
    in
    svg [ viewBox 0 0 w h, PX.width w, PX.height h ]


viewLine : Int2 -> Int2 -> Svg msg
viewLine p1 p2 =
    let
        ( x1, y1 ) =
            p1 |> (NT.toFloat >> NT.scale cellSize >> NT.add ( cellSize / 2, cellSize / 2 ))

        ( x2, y2 ) =
            p2 |> (NT.toFloat >> NT.scale cellSize >> NT.add ( cellSize / 2, cellSize / 2 ))
    in
    line
        [ PX.x1 x1
        , PX.y1 y1
        , PX.x2 x2
        , PX.y2 y2
        , stroke Color.black
        ]
        []


viewEndPoint : Int2 -> Svg msg
viewEndPoint p1 =
    let
        ( x1, y1 ) =
            p1 |> (NT.toFloat >> NT.scale cellSize)
    in
    circle
        [ PX.cx x1
        , PX.cy y1
        , PX.r (cellSize / 10)
        , Svg.Attributes.fillOpacity "0.5"
        , transform [ Translate (cellSize / 2) (cellSize / 2) ]
        ]
        []


viewLightTree : ( Int2, List (Tree Int2) ) -> List (Svg msg)
viewLightTree ( start, trees ) =
    List.filterMap Tree.root trees
        |> List.concatMap (\( end, newTrees ) -> viewLine start end :: viewLightTree ( end, newTrees ))


viewLightForest : List (Tree Int2) -> List (Svg msg)
viewLightForest =
    List.filterMap Tree.root >> List.concatMap viewLightTree


viewGridItem : ( Int2, b ) -> Svg msg
viewGridItem ( position, _ ) =
    let
        ( x, y ) =
            position |> NT.toFloat |> NT.scale cellSize
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


type alias Grid =
    Dict Int2 El


grid : Dict Int2 El
grid =
    gr |> Tuple.second



{-
   gr =
       Dict2d.fromListsWithDefault Continue
           [ [ Continue, Continue, Continue, Split [ D.left ] ]
           , [ Start [ D.right ], Continue, Continue, Split [ D.down, D.up ] ]
           , [ Split [ D.right ], Continue, Continue, Split [ D.left ] ]
           ]
-}


gr =
    Dict2d.fromListsWithDefault Continue
        [ [ Continue, Continue, Split [ D.up ], Split [ D.left ] ]
        , [ Continue, Start [ D.right ], Continue, Split [ D.up, D.down ] ]
        , [ Split [ D.down ], Continue, Continue, Split [ D.left ] ]
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


unpackForest : List (Tree label) -> List ( label, Forest label )
unpackForest =
    List.filterMap Tree.root


type alias Acc =
    ( EdgesEndPoints, List ( Int2, Forest Int2 ) )


type alias EdgesEndPoints =
    ( Dict Int2 Int2, Set Int2 )


lightForestEdgesEndpoints : Forest Int2 -> EdgesEndPoints
lightForestEdgesEndpoints =
    let
        accumTreeEdges : Acc -> EdgesEndPoints
        accumTreeEdges ( ( dict, endPoints ), list ) =
            case list of
                [] ->
                    ( dict, endPoints )

                first :: rest ->
                    let
                        ( parentIndex, childForest ) =
                            Tuple.mapSecond unpackForest first

                        newDict =
                            List.foldl
                                (\( childIndex, _ ) ->
                                    Dict.insert childIndex parentIndex
                                )
                                dict
                                childForest

                        newEndPoints =
                            if List.isEmpty childForest then
                                Set.insert parentIndex endPoints

                            else
                                endPoints
                    in
                    accumTreeEdges ( ( newDict, newEndPoints ), childForest ++ rest )
    in
    unpackForest
        >> (\forest -> accumTreeEdges ( ( Dict.empty, Set.empty ), forest ))


type alias PathNode comparable a =
    ( comparable, a )


type alias NextPathNodes comparable a =
    PathNode comparable a -> List (PathNode comparable a)


type alias Graph comparable =
    ( Set ( comparable, comparable ), Set comparable )


foo : Grid -> NextPathNodes Int2 (List Direction8)
foo grid0 ( prevPosition, previousDirections ) =
    let
        nextPathNodeInDirection : Direction8 -> Maybe (PathNode Int2 (List Direction8))
        nextPathNodeInDirection direction =
            let
                position =
                    D.stepPos direction prevPosition
            in
            (case Dict.get position grid0 of
                Just el ->
                    case el of
                        Split directions ->
                            Just directions

                        End ->
                            Nothing

                        Start _ ->
                            Just [ direction ]

                        Continue ->
                            Just [ direction ]

                Nothing ->
                    Nothing
            )
                |> Maybe.map (Tuple.pair position)
    in
    List.filterMap nextPathNodeInDirection previousDirections


lightPathGraphs : Grid -> List (Graph Int2)
lightPathGraphs grid0 =
    let
        toLightPathGraph ( position, el ) =
            case el of
                Start dirs ->
                    unfoldGraph (foo grid0) ( position, dirs )
                        |> Just

                _ ->
                    Nothing
    in
    Dict.toList grid0 |> List.filterMap toLightPathGraph


unfoldGraph : NextPathNodes comparable a -> PathNode comparable a -> Graph comparable
unfoldGraph nextPathNodeContextsFunc =
    let
        gen : Graph comparable -> List (PathNode comparable a) -> Graph comparable
        gen acc0 nodeContexts0 =
            case nodeContexts0 of
                [] ->
                    acc0

                nodeContext0 :: nodeContexts1 ->
                    let
                        parent =
                            Tuple.first nodeContext0

                        nodeContexts2 : List (PathNode comparable a)
                        nodeContexts2 =
                            nextPathNodeContextsFunc nodeContext0

                        ( acc2, nodeContexts4 ) =
                            nodeContexts2
                                |> List.foldl
                                    (\nodeContext1 (( ( edges, endPoints ), nodeContexts3 ) as acc1) ->
                                        let
                                            child =
                                                Tuple.first nodeContext1

                                            edge =
                                                ( child, parent )
                                        in
                                        if Set.member edge edges || Set.member (swap edge) edges then
                                            -- on cyclic graph, we are currently adding an endpoint
                                            -- alternatively, we could just ignore ep, on cycle.
                                            ( ( edges, Set.insert parent endPoints ), nodeContexts3 )

                                        else
                                            ( ( Set.insert edge edges, endPoints ), nodeContext1 :: nodeContexts3 )
                                    )
                                    ( acc0, nodeContexts1 )
                    in
                    if List.isEmpty nodeContexts2 then
                        gen (Tuple.mapSecond (Set.insert parent) acc0) nodeContexts1

                    else
                        gen acc2 nodeContexts4
    in
    List.singleton >> gen ( Set.empty, Set.empty )
