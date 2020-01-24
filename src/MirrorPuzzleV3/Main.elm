module MirrorPuzzleV3.Main exposing (main)

import Color
import Html exposing (Html)
import Html.Attributes exposing (class)
import MirrorPuzzleV3.ElGrid as ElGrid
import MirrorPuzzleV3.Graph as Graph exposing (Graph)
import MirrorPuzzleV3.Tile as Tile
import MirrorPuzzleV3.TileGird as TileGrid
import Number2 as NT exposing (Int2)
import Playground.Direction8 as D exposing (Direction8)
import Set exposing (Set)
import Svg exposing (Svg, ellipse, g)
import Svg.Attributes
import TypedSvg exposing (circle, line, rect, svg)
import TypedSvg.Attributes exposing (fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx as PX
import TypedSvg.Types exposing (Fill(..), Transform(..))



-- Main


main =
    let
        elGrid =
            ElGrid.initial

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
            , ElGrid.view elGrid
            ]
        ]



-- TileGrid


initialTileGrid : TileGrid.TileGrid
initialTileGrid =
    [ [ Tile.Hole, Tile.Wall, Tile.Wall, Tile.Wall, Tile.Wall, Tile.Hole ]
    , [ Tile.Wall, Tile.floor, Tile.floor, Tile.floor, Tile.floor, Tile.Wall ]
    , [ Tile.Wall, Tile.floor, Tile.floor, Tile.mirror D.down, Tile.mirror (D.rotate 1 D.left), Tile.Wall ]
    , [ Tile.Wall, Tile.floor, Tile.lightSourceWithMirror D.right, Tile.floor, Tile.prism D.up, Tile.Wall ]
    , [ Tile.Wall, Tile.mirror D.up, Tile.floor, Tile.floor, Tile.mirror D.left, Tile.Wall ]
    , [ Tile.Wall, Tile.floor, Tile.floor, Tile.floor, Tile.floor, Tile.Wall ]
    ]
        |> List.reverse
        |> TileGrid.fromList2d


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
            svg
                [ Svg.Attributes.class "cartesian"
                , viewBox (-w / 2) (-h / 2) w h
                , PX.width w
                , PX.height h
                , Svg.Attributes.preserveAspectRatio "xMidYMid meet"
                ]
                [ g [] children ]
    in
    let
        floorForm =
            rect
                [ PX.width cellSize
                , PX.height cellSize
                , Svg.Attributes.stroke "gray"
                , fill FillNone
                ]
                []

        wallForm =
            rect
                [ PX.width cellSize
                , PX.height cellSize
                , Svg.Attributes.fill "silver"
                , Svg.Attributes.stroke "silver"
                ]
                []

        cc =
            cellSize / 2

        mirrorForm direction =
            ellipse
                [ PX.rx (cellSize / 4)
                , PX.ry cellSize
                , PX.cx (cc - cellSize / 4)
                , PX.cy cc
                , Svg.Attributes.fill "lightblue"
                , Svg.Attributes.stroke "none"
                , transform
                    [ Translate cc cc
                    , Scale 0.45 0.45
                    , Translate -cc -cc
                    , Rotate (direction |> D.toDegrees) cc cc
                    ]
                ]
                []

        tileForm _ tile =
            case tile of
                Tile.FilledContainer _ element ->
                    case element.type_ of
                        Tile.Mirror ->
                            [ floorForm
                            , mirrorForm element.direction
                            ]

                        _ ->
                            [ floorForm
                            ]

                Tile.EmptyContainer _ ->
                    [ floorForm ]

                Tile.Goal ->
                    [ floorForm ]

                Tile.Wall ->
                    [ wallForm ]

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
        renderGridCells =
            TileGrid.toList grid
                |> List.map renderCell
    in
    let
        renderGraphs =
            TileGrid.computeLightPaths grid
                |> List.concatMap (viewGraph { cellSize = cellSize })
    in
    renderGridContent
        [ g
            [ let
                ( w, h ) =
                    TileGrid.dimensions grid |> NT.toFloat |> NT.scale cellSize
              in
              transform [ Translate (-w / 2) (-h / 2) ]
            ]
            (renderGridCells ++ renderGraphs)
        ]



-- Model
-- View


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
