module MirrorPuzzleV3.Main exposing (main)

import Basics.Extra exposing (uncurry)
import Color
import Dict exposing (Dict)
import Dict2d
import Html exposing (Html)
import Html.Attributes exposing (class)
import MirrorPuzzleV3.Graph as Graph
import Number2 as NT exposing (Int2)
import Playground.Direction8 as D exposing (Direction8)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes
import TypedSvg exposing (circle, line, rect, svg)
import TypedSvg.Attributes exposing (fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx as PX
import TypedSvg.Types exposing (Fill(..), Transform(..))


main =
    let
        gv =
            initialGridView
    in
    Html.div [ class "pa2 inline-flex flex-wrap" ]
        [ Html.div [ class "inline-flex flex-column" ]
            [ Html.div [ class "tc pa2" ] [ Html.text "Grid" ]
            , canvas2d gv
                ((gv.grid |> Dict.toList |> List.map (viewGridCell gv))
                    ++ viewNewLightPathGraphs gv (lightPathGraphs gv.grid)
                )
            ]
        ]


type alias GridView =
    { grid : ElGrid
    , dimensions : Int2
    , cellSize : Float
    }


initialGridView : GridView
initialGridView =
    let
        ( dimensions, grid_ ) =
            Dict2d.fromListsWithDefault Continue
                [ [ Continue, Continue, Split [ D.down ], Split [ D.left ] ]
                , [ Continue, Start [ D.right ], Continue, Split [ D.up, D.down ] ]
                , [ Split [ D.up ], Continue, Continue, Split [ D.left ] ]
                ]
    in
    GridView grid_ dimensions 100


canvas2d : { a | dimensions : Int2, cellSize : Float } -> List (Svg msg) -> Html msg
canvas2d gv children =
    let
        ( w, h ) =
            gv.dimensions |> NT.toFloat |> NT.scale gv.cellSize
    in
    Html.div [ class "pa2 flex" ] [ svg [ viewBox 0 0 w h, PX.width w, PX.height h ] children ]


viewNewLightPathGraphs : GridView -> List Graph.Graph -> List (Svg msg)
viewNewLightPathGraphs gv =
    let
        foo graph =
            List.map (uncurry (viewLine gv)) (Set.toList (Graph.getEdges graph))
                ++ List.map (viewEndPoint gv) (Set.toList (Graph.getEndPoints graph))
    in
    List.concatMap foo


viewLine : GridView -> Int2 -> Int2 -> Svg msg
viewLine { cellSize } p1 p2 =
    let
        transformPoint =
            NT.toFloat >> NT.scale cellSize >> NT.add ( cellSize / 2, cellSize / 2 )

        ( x1, y1 ) =
            transformPoint p1

        ( x2, y2 ) =
            transformPoint p2
    in
    line
        [ PX.x1 x1
        , PX.y1 y1
        , PX.x2 x2
        , PX.y2 y2
        , stroke Color.black
        ]
        []


viewEndPoint : GridView -> Int2 -> Svg msg
viewEndPoint { cellSize } p1 =
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


viewGridCell : GridView -> ( Int2, b ) -> Svg msg
viewGridCell { cellSize } ( position, _ ) =
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


type alias ElGrid =
    Dict Int2 El


lightPathGraphs : ElGrid -> List Graph.Graph
lightPathGraphs grid =
    let
        unfoldInstructionAt : Int2 -> Graph.UnfoldInstruction
        unfoldInstructionAt position =
            case Dict.get position grid of
                Just el ->
                    case el of
                        Split directions ->
                            Graph.Fork directions

                        End ->
                            Graph.Stop

                        Start _ ->
                            Graph.ContinuePrevious

                        Continue ->
                            Graph.ContinuePrevious

                Nothing ->
                    Graph.Stop

        graphStartingAt : ( Int2, El ) -> Maybe Graph.Graph
        graphStartingAt ( position, el ) =
            case el of
                Start dirs ->
                    Just (Graph.unfold unfoldInstructionAt position dirs)

                {- Graph.unfoldGraph
                   (nextLightPathNode elGrid)
                   ( position, dirs )
                   |> Just
                -}
                _ ->
                    Nothing
    in
    Dict.toList grid |> List.filterMap graphStartingAt
