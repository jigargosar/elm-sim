module MirrorPuzzleV3.Main exposing (main)

import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (..)
import Collage.Text as Text
import Color
import Html exposing (Html)
import Html.Attributes exposing (class)
import MirrorPuzzleV3.ElGrid as ElGrid
import MirrorPuzzleV3.Tile as Tile
import MirrorPuzzleV3.TileGird as TileGrid
import MirrorPuzzleV3.TileGridDemo as TileGridDemo
import Number2 as NT
import Playground.Direction8 as D



-- Main


main =
    Html.div [ class "pa2 inline-flex flex-wrap" ]
        [ Html.div [ class "inline-flex flex-column" ]
            [ Html.div [ class "tc pa2" ] [ Html.text "collageDemo" ]
            , Html.div [ class "pa4" ] [ collageDemo ]
            ]
        , Html.div [ class "inline-flex flex-column" ]
            [ Html.div [ class "tc pa2" ] [ Html.text "TileGrid" ]
            , TileGridDemo.viewDemo
            ]
        , Html.div [ class "inline-flex flex-column" ]
            [ Html.div [ class "tc pa2" ] [ Html.text "Grid" ]
            , ElGrid.viewDemo
            ]
        ]


collageDemo : Html msg
collageDemo =
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

        viewCellLayer =
            initialTileGrid
                |> TileGrid.toList
                |> List.map viewGridCell
                |> group

        shiftCellAt position =
            shift (position |> NT.toFloat |> NT.scale 50)

        viewGridCell ( position, _ ) =
            [ square 50
                |> outlined (solid thin (uniform Color.gray))

            --|> debug
            , Debug.toString position
                |> Text.fromString
                --|> Text.size Text.normal
                |> rendered
            ]
                |> stack
                |> shiftCellAt position
    in
    viewCellLayer |> svg
