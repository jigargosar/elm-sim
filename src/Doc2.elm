module Doc2 exposing (viewSampleDoc)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


type Doc
    = Doc (List Tree)


type Tree
    = Tree String (List Tree)


empty : Doc
empty =
    Doc []


newTree : String -> Tree
newTree string =
    Tree string []


appendSibling : String -> Doc -> Doc
appendSibling string (Doc list) =
    Doc (list ++ [ newTree string ])


insertChild : String -> Doc -> Doc
insertChild string doc =
    appendSibling string doc


viewDoc : Doc -> Html msg
viewDoc (Doc list) =
    div [ class "pa3" ]
        [ div [ class "f4 b pv2" ] [ text "Doc" ]
        , List.map viewTreeData list
            |> div []
        ]


viewTreeData : Tree -> Html msg
viewTreeData (Tree string _) =
    div [] [ text string ]


viewSampleDoc : Html msg
viewSampleDoc =
    let
        sampleDoc : Doc
        sampleDoc =
            empty
                |> appendSibling "First"
                |> appendSibling "Secomd"
                |> appendSibling "Third"
                |> insertChild "Third's Child 1"
    in
    viewDoc sampleDoc
