module MirrorPuzzleV3.TileGridCollageDemo exposing (viewDemo)

import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (..)
import Collage.Text as Text
import Color
import Html exposing (Html)
import MirrorPuzzleV3.Graph as Graph
import MirrorPuzzleV3.Tile as Tile
import MirrorPuzzleV3.TileGird as TileGrid
import Number2 as NT exposing (Float2, Int2)
import Playground.Direction8 as D
import PointFree exposing (mapEach)
import Set



-- Main


viewDemo : Html msg
viewDemo =
    let
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

        cellW =
            100

        toViewPosition position =
            position |> NT.toFloat |> NT.scale cellW

        viewCellLayer =
            initialTileGrid
                |> TileGrid.toList
                |> List.map (viewGridCell cellW toViewPosition)
                |> stack

        viewLightPathLayer =
            initialTileGrid
                |> TileGrid.computeLightPaths
                |> List.concatMap (viewLightPath cellW toViewPosition)
                |> stack
    in
    [ viewLightPathLayer, viewCellLayer ]
        |> stack
        --|> debug
        |> svg


viewGridCell : Float -> (Int2 -> Float2) -> ( Int2, Tile.Tile ) -> Collage msg
viewGridCell cellW toViewPosition ( position, tile ) =
    let
        silver =
            uniform <| Color.rgb255 192 192 192

        floorShape =
            square cellW
                |> outlined (solid 0.5 silver)

        mirrorShape d =
            ellipse (cellW / 8) (cellW / 2)
                |> filled (uniform Color.lightBlue)
                |> shiftX (-cellW / 8)
                |> List.singleton
                |> stack
                |> scale 0.9
                |> List.singleton
                |> stack
                |> rotate (D.toRadians d)

        tileShape =
            case tile of
                Tile.FilledContainer _ element ->
                    case element.type_ of
                        Tile.Mirror ->
                            [ mirrorShape element.direction
                            , floorShape
                            ]
                                |> stack

                        _ ->
                            floorShape

                Tile.Wall ->
                    [ square cellW |> filled silver
                    , floorShape
                    ]
                        |> stack

                _ ->
                    floorShape
    in
    [ Debug.toString position
        |> Text.fromString
        --|> Text.size Text.normal
        |> rendered
        |> opacity 0.3

    --|> debug
    , tileShape
    ]
        |> stack
        |> shift (toViewPosition position)


viewLightPath : Float -> (Int2 -> Float2) -> Graph.Graph -> List (Collage msg)
viewLightPath cellW toViewPosition graph =
    let
        viewEdges =
            Graph.getEdges graph
                |> Set.toList
                |> List.map viewEdge

        viewEndPoints =
            Graph.getEndPoints graph
                |> Set.toList
                |> List.map viewEndPoint

        viewEndPoint : NT.Int2 -> Collage msg
        viewEndPoint ep =
            endPointShape |> shift (toViewPosition ep)

        endPointShape : Collage msg
        endPointShape =
            circle (cellW / 8) |> filled (uniform Color.black) |> opacity 0.5

        viewEdge : ( NT.Int2, NT.Int2 ) -> Collage msg
        viewEdge points =
            let
                ( p1, p2 ) =
                    mapEach toViewPosition points
            in
            segment p1 p2
                |> traced (solid thin (uniform Color.black))
                |> opacity 0.5
    in
    [ viewEndPoints
    , viewEdges
    ]
        |> List.concat
