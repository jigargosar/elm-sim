module Doc2 exposing (viewSampleDoc)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


type Doc
    = Doc (List String)


empty : Doc
empty =
    Doc []


appendSibling : String -> Doc -> Doc
appendSibling string (Doc list) =
    Doc (string :: list)


viewDoc : Doc -> Html msg
viewDoc (Doc list) =
    div [ class "pa3" ]
        [ div [ class "f4 b pv2" ] [ text "Doc" ]
        , List.map viewNode list
            |> div []
        ]


viewNode string =
    div [] [ text string ]


viewSampleDoc : Html msg
viewSampleDoc =
    let
        sampleDoc : Doc
        sampleDoc =
            empty
                |> appendSibling "First"
    in
    viewDoc sampleDoc
