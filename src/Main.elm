module Main exposing (main)

import Dict
import Html as H exposing (Html, div)
import Html.Attributes exposing (class, style)


main : Html msg
main =
    div [ flexCenter, fixedFullscreen ]
        [ renderGlobalStyles
        , renderGrid 4
        ]


grid : Dict.Dict ( Int, Int ) Int
grid =
    let
        entry r c =
            ( ( r, c ), 0 )
    in
    times 4 (\r -> times 4 (entry r))
        |> List.concat
        |> Dict.fromList


renderGrid : Int -> Html msg
renderGrid size =
    let
        renderIndexedCell rowIdx colIdx =
            let
                cellContent =
                    String.fromInt (rowIdx + 1) ++ "," ++ String.fromInt (colIdx + 1)

                cellNumContent =
                    Dict.get ( rowIdx, colIdx ) grid
                        |> Maybe.map String.fromInt
                        |> Maybe.withDefault "ERR"
            in
            renderCell cellNumContent

        renderRow rIdx =
            rowLayout (times size (renderIndexedCell rIdx))
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
