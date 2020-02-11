module AllItemsListView exposing (viewItemsList)

import DataModels exposing (..)
import Html exposing (Html, input, text)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import String as S
import ViewHelpers exposing (..)


type alias Config msg =
    { onTitleClick : Item -> msg
    }


viewItemsList : Config msg -> List Item -> Html msg
viewItemsList config items =
    col [] (List.map (viewItem config) items)


viewItem : Config msg -> Item -> Html msg
viewItem { onTitleClick } item =
    let
        ( displayTitle, colorClass ) =
            if (S.trim >> S.isEmpty) item.title then
                ( "<empty>", "fg-gray" )

            else
                ( item.title, "fg-inherit" )
    in
    row [ class colorClass ]
        [ el [ class "p5" ] (input [ type_ "checkbox", class "p5" ] [])
        , el [ class "p5 fg1", onClick (onTitleClick item) ] (text displayTitle)
        ]
