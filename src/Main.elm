module Main exposing (main)

import Dict exposing (Dict)
import Html as H exposing (Html, div)
import Html.Attributes exposing (class, style)


main : Html msg
main =
    let
        grid =
            initGrid 4
    in
    div [ flexCenter, fixedFullscreen ]
        [ renderGlobalStyles
        , renderGrid grid
        ]


type Grid
    = Grid Int (Dict ( Int, Int ) Cell)


type alias Cell =
    Int


initGrid : Int -> Grid
initGrid size =
    let
        dict =
            times size (\r -> times size (Tuple.pair r))
                |> List.concat
                |> List.indexedMap (\i rc -> ( rc, i + 1 ))
                |> Dict.fromList
    in
    Dict.insert ( size - 1, size - 1 ) -1 dict
        |> Grid size


getGridRow : Int -> Grid -> List Cell
getGridRow n (Grid _ dict) =
    Dict.filter (\( r, _ ) _ -> r == n) dict
        |> Dict.values


getGridSize : Grid -> Int
getGridSize (Grid size _) =
    size


renderGrid : Grid -> Html msg
renderGrid grid =
    let
        renderRow rIdx =
            getGridRow rIdx grid
                |> List.map (String.fromInt >> renderCell)
                |> rowLayout
    in
    columnLayout (times (getGridSize grid) renderRow)


rowLayout =
    div [ flex ]


columnLayout =
    div [ flex, flexColumn ]


renderCell cellContent =
    div
        [ style "width" "200px"
        , style "height" "200px"
        , style "background-color" "gray"
        , style "font-size" "80px"
        , style "font-family" "monospace"
        , style "border" "1px solid black"
        , flexCenter
        ]
        [ H.text cellContent ]


times n func =
    List.range 0 (n - 1) |> List.map func



-- STYLES


renderGlobalStyles =
    H.node "style" [] [ H.text globalStyles ]


globalStyles =
    """
        .flex-center{
            display: flex;
            align-items: center;
            justify-content: center;
        }

        .fixed-fullscreen {
            position: fixed;
            width: 100%;
            height: 100%;
            top: 0;
            left: 0;
        }
    """


flexCenter =
    class "flex-center"


fixedFullscreen =
    class "fixed-fullscreen"


flex =
    style "display" "flex"


flexColumn =
    style "flex-direction" "column"
