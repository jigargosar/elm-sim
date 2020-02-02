module Main exposing (main)

import Html as H exposing (Html, div)
import Html.Attributes exposing (class, style)


main : Html msg
main =
    div [ flexCenter, fixedFullscreen ]
        [ renderGlobalStyles
        , renderCell
        ]


renderCell =
    div
        [ style "width" "100px"
        , style "height" "100px"
        , style "background-color" "gray"
        , style "font-size" "80px"
        , style "font-family" "monospace"
        , flexCenter
        ]
        [ H.text "0" ]



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
