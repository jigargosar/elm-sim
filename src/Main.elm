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


type Cell
    = Num Int
    | Empty


initGrid : Int -> Grid
initGrid size =
    let
        dict =
            times size (\r -> times size (Tuple.pair r))
                |> List.concat
                |> List.indexedMap (\i rc -> ( rc, Num (i + 1) ))
                |> Dict.fromList
    in
    Dict.insert ( size - 1, size - 1 ) Empty dict
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
                |> List.map renderCell
                |> rowLayout
    in
    div [ flex, flexColumn, cellBorder ]
        (times (getGridSize grid) renderRow)


rowLayout =
    div [ flex ]


renderCell : Cell -> Html msg
renderCell cell =
    case cell of
        Num num ->
            renderCellString (String.fromInt num)
                [ style "background-color" "gray"
                ]

        Empty ->
            renderCellString "" []


renderCellString cellContent attrs =
    div
        ([ style "width" "200px"
         , style "height" "200px"
         , cellBorder
         , style "font-size" "80px"
         , style "font-family" "monospace"
         , flexCenter
         ]
            ++ attrs
        )
        [ H.text cellContent ]


cellBorder =
    style "border" "1px solid black"


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
