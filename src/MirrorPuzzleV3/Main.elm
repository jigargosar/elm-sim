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
import PointFree exposing (mapEach)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes
import TypedSvg exposing (circle, line, rect, svg)
import TypedSvg.Attributes exposing (fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx as PX
import TypedSvg.Types exposing (Fill(..), Transform(..))


main =
    Html.div [ class "pa2 inline-flex flex-wrap" ]
        [ Html.div [ class "inline-flex flex-column" ]
            [ Html.div [ class "tc pa2" ] [ Html.text "Graph.unfoldDirection8Graph movementAt" ]
            , gridCanvasWith
                (viewNewLightPathGraphs (lightPathGraphs grid))
            ]
        ]


type alias GridView =
    { grid : ElGrid
    , cellSize : Float
    , dimensions : Int2
    }


cellSize =
    100


viewNewLightPathGraphs : List Graph.Graph -> List (Svg msg)
viewNewLightPathGraphs =
    let
        foo graph =
            List.map (uncurry viewLine) (Set.toList (Graph.getEdges graph))
                ++ List.map viewEndPoint (Set.toList (Graph.getEndPoints graph))
    in
    List.concatMap foo


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


type alias ElGrid =
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
        [ [ Continue, Continue, Split [ D.down ], Split [ D.left ] ]
        , [ Continue, Start [ D.right ], Continue, Split [ D.up, D.down ] ]
        , [ Split [ D.up ], Continue, Continue, Split [ D.left ] ]
        ]


gridDimensions =
    gr |> Tuple.first


gridDimensionsF =
    gridDimensions |> mapEach toFloat


lightPathGraphs : ElGrid -> List Graph.Graph
lightPathGraphs elGrid =
    let
        unfoldInstructionAt : Int2 -> Graph.UnfoldInstruction
        unfoldInstructionAt position =
            case Dict.get position elGrid of
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
    Dict.toList elGrid |> List.filterMap graphStartingAt
