module Main exposing (main)

import Dict exposing (Dict)
import Html as H exposing (Html, div)
import Html.Attributes exposing (class, style)


main : Html msg
main =
    div [ flexCenter, fixedFullscreen ]
        [ renderGlobalStyles
        , renderGrid 4 (initGrid 4)
        ]


initGrid size =
    let
        dict =
            times size (\r -> times size (Tuple.pair r))
                |> List.concat
                |> List.indexedMap (\i rc -> ( rc, i + 1 ))
                |> Dict.fromList
    in
    Dict.insert ( size - 1, size - 1 ) -1 dict


getGridRow n grid =
    Dict.filter (\( r, _ ) _ -> r == n) grid
        |> Dict.values


renderGrid : Int -> Dict ( Int, Int ) Int -> Html msg
renderGrid size grid =
    let
        renderRow rIdx =
            getGridRow rIdx grid
                |> List.map (String.fromInt >> renderCell)
                |> rowLayout
    in
    columnLayout (times size renderRow)


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
