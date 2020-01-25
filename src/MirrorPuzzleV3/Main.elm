module MirrorPuzzleV3.Main exposing (main)

import Html exposing (Html)
import Html.Attributes exposing (class)
import MirrorPuzzleV3.SimpleGridDemo as SimpleGridDemo
import MirrorPuzzleV3.TileGridCollageDemo as TileGridCollageDemo
import MirrorPuzzleV3.TileGridSvgDemo as TileGridDemo



-- Main


main =
    Html.div [ class "pa2 inline-flex flex-wrap" ]
        [ Html.div [ class "inline-flex flex-column" ]
            [ Html.div [ class "tc pa2" ] [ Html.text "collageDemo" ]
            , Html.div [ class "pa4 lh-0" ] [ TileGridCollageDemo.viewDemo ]
            ]
        , Html.div [ class "inline-flex flex-column" ]
            [ Html.div [ class "tc pa2" ] [ Html.text "TileGrid" ]
            , TileGridDemo.viewDemo
            ]
        , Html.div [ class "inline-flex flex-column" ]
            [ Html.div [ class "tc pa2" ] [ Html.text "Grid" ]
            , SimpleGridDemo.viewDemo
            ]
        ]
