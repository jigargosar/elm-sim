module MirrorPuzzleV3.Main exposing (main)

import Color
import Dict exposing (Dict)
import Dict2d
import Html exposing (Html)
import Html.Attributes exposing (class)
import MirrorPuzzleV3.Graph as Graph exposing (Graph)
import MirrorPuzzleV3.Tile as Tile
import MirrorPuzzleV3.TileGird as TileGrid
import Number2 as NT exposing (Int2)
import Playground.Direction8 as D exposing (Direction8)
import Set exposing (Set)
import Svg exposing (Svg, g)
import Svg.Attributes
import TypedSvg exposing (circle, line, rect, svg)
import TypedSvg.Attributes exposing (fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx as PX
import TypedSvg.Types exposing (Fill(..), Transform(..))



-- Main


main =
    let
        elGrid =
            initialElGrid

        tileGrid =
            initialTileGrid
    in
    Html.div [ class "pa2 inline-flex flex-wrap" ]
        [ Html.div [ class "inline-flex flex-column" ]
            [ Html.div [ class "tc pa2" ] [ Html.text "TileGrid" ]
            , viewTileGrid tileGrid
            ]
        , Html.div [ class "inline-flex flex-column" ]
            [ Html.div [ class "tc pa2" ] [ Html.text "Grid" ]
            , canvas elGrid
                ((elGrid.dict |> Dict.toList |> List.map (viewGridCell elGrid))
                    ++ viewLightPaths elGrid
                )
            ]
        ]



-- TileGrid


initialTileGrid : TileGrid.TileGrid
initialTileGrid =
    TileGrid.fromList2d
        [ [ Tile.Wall, Tile.Wall, Tile.Wall, Tile.Wall, Tile.Wall, Tile.Wall ]
        , [ Tile.Wall, Tile.floor, Tile.floor, Tile.mirror D.down, Tile.mirror D.left, Tile.Wall ]
        , [ Tile.Wall, Tile.floor, Tile.floor, Tile.mirror D.down, Tile.mirror D.left, Tile.Wall ]
        , [ Tile.Wall, Tile.floor, Tile.lightSourceWithMirror D.right, Tile.floor, Tile.prism D.up, Tile.Wall ]
        , [ Tile.Wall, Tile.mirror D.up, Tile.floor, Tile.floor, Tile.mirror D.left, Tile.Wall ]
        , [ Tile.Wall, Tile.floor, Tile.floor, Tile.floor, Tile.floor, Tile.Wall ]
        ]


viewTileGrid grid =
    let
        cellSize =
            100
    in
    let
        renderGridContent children =
            let
                ( w, h ) =
                    TileGrid.dimensions grid |> NT.toFloat |> NT.scale cellSize
            in
            Html.div [ class "pa2 flex" ] [ svg [ viewBox 0 0 w h, PX.width w, PX.height h ] children ]
    in
    let
        floorForm =
            rect
                [ PX.width cellSize
                , PX.height cellSize
                , stroke Color.black
                , fill FillNone
                ]
                []

        tileForm position tile =
            case tile of
                Tile.FilledContainer _ _ ->
                    [ floorForm
                    ]

                Tile.EmptyContainer elementContainer ->
                    [ floorForm ]

                Tile.Goal ->
                    [ floorForm ]

                Tile.Wall ->
                    [ floorForm ]

                Tile.Hole ->
                    []

        renderCell : ( Int2, Tile.Tile ) -> Svg msg
        renderCell ( position, tile ) =
            let
                ( x, y ) =
                    position |> NT.toFloat |> NT.scale cellSize
            in
            tileForm position tile
                |> g [ transform [ Translate x y ] ]
    in
    let
        renderGridCells : Svg msg
        renderGridCells =
            TileGrid.toList grid
                |> List.map renderCell
                |> g []
    in
    let
        renderGraphs =
            TileGrid.computeLightPaths grid
                |> List.concatMap (viewGraph { cellSize = cellSize })
                |> g []
    in
    renderGridContent [ renderGridCells, renderGraphs ]



-- Model


type alias ElGrid =
    { dict : ElDict
    , dimensions : Int2
    , cellSize : Float
    }


initialElGrid : ElGrid
initialElGrid =
    let
        ( dimensions, grid_ ) =
            Dict2d.fromListsWithDefault Continue
                [ [ Continue, Continue, Split [ D.down ], Split [ D.left ] ]
                , [ Continue, Start [ D.right ], Continue, Split [ D.up, D.down ] ]
                , [ Split [ D.up ], Continue, Continue, Split [ D.left ] ]
                ]
    in
    ElGrid grid_ dimensions 100


toLightPathGraphs : ElDict -> List Graph.Graph
toLightPathGraphs elDict =
    let
        unfoldInstructionAt : Int2 -> Graph.UnfoldInstruction
        unfoldInstructionAt position =
            case Dict.get position elDict of
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

                _ ->
                    Nothing
    in
    Dict.toList elDict |> List.filterMap graphStartingAt



-- View


canvas : { a | dimensions : Int2, cellSize : Float } -> List (Svg msg) -> Html msg
canvas gv children =
    let
        ( w, h ) =
            gv.dimensions |> NT.toFloat |> NT.scale gv.cellSize
    in
    Html.div [ class "pa2 flex" ] [ svg [ viewBox 0 0 w h, PX.width w, PX.height h ] children ]


viewLightPaths : ElGrid -> List (Svg msg)
viewLightPaths elGrid =
    List.concatMap (viewGraph elGrid) (toLightPathGraphs elGrid.dict)


viewGraph : { a | cellSize : Float } -> Graph -> List (Svg msg)
viewGraph elGrid graph =
    viewGraphEdges elGrid graph ++ viewGraphEndPoints elGrid graph


viewGraphEdges : { a | cellSize : Float } -> Graph -> List (Svg msg)
viewGraphEdges elGrid graph =
    List.map (viewEdge elGrid) (Graph.getEdges graph |> Set.toList)


viewGraphEndPoints : { a | cellSize : Float } -> Graph -> List (Svg msg)
viewGraphEndPoints elGrid graph =
    List.map (viewEndPoint elGrid) (Graph.getEndPoints graph |> Set.toList)


viewEdge : { a | cellSize : Float } -> ( Int2, Int2 ) -> Svg msg
viewEdge grid ( p1, p2 ) =
    viewLine grid p1 p2


viewLine : { a | cellSize : Float } -> Int2 -> Int2 -> Svg msg
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


viewEndPoint : { a | cellSize : Float } -> Int2 -> Svg msg
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


viewGridCell : ElGrid -> ( Int2, b ) -> Svg msg
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


type alias ElDict =
    Dict Int2 El
