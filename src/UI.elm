module UI exposing (..)

import Class
import Html exposing (..)
import Html.Attributes exposing (classList)


shouldDebugLayout =
    False


hStack : List (Attribute msg) -> List (Html msg) -> Html msg
hStack lst =
    Class.dFlex
        :: Class.fdRow
        :: Class.justifyCenter
        :: classList [ ( "layout-debug", shouldDebugLayout ) ]
        :: lst
        |> div


vStack : List (Attribute msg) -> List (Html msg) -> Html msg
vStack lst =
    Class.dFlex
        :: Class.fdCol
        :: Class.justifyCenter
        :: classList [ ( "layout-debug", shouldDebugLayout ) ]
        :: lst
        |> div
