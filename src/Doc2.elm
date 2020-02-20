module Doc2 exposing (viewSampleDoc)

import Html exposing (Html, div, text)


type Doc
    = Doc String
    | EmptyDoc


empty : Doc
empty =
    EmptyDoc


appendSibling : String -> Doc -> Doc
appendSibling string doc =
    Doc string


viewDoc : Doc -> Html msg
viewDoc doc =
    div [] [ text "Doc" ]


viewSampleDoc : Html msg
viewSampleDoc =
    let
        sampleDoc : Doc
        sampleDoc =
            empty
                |> appendSibling "First"
    in
    viewDoc sampleDoc
