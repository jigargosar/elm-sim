module Doc2 exposing (viewSampleDoc)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)



-- LCR


type LCR
    = LCR ( List String, String, List String )


initLCR : String -> LCR
initLCR string =
    LCR ( [], string, [] )


appendAfterC : String -> LCR -> LCR
appendAfterC string (LCR ( l, c, r )) =
    LCR ( c :: l, string, r )


toList : LCR -> List String
toList (LCR ( l, c, r )) =
    List.reverse l ++ c :: r


viewLCR : LCR -> List (Html msg)
viewLCR lcr =
    let
        viewS string =
            div [] [ text string ]
    in
    toList lcr |> List.map viewS



-- DOC


type Doc
    = Doc LCR
    | Empty


empty : Doc
empty =
    Empty


appendSibling : String -> Doc -> Doc
appendSibling string doc =
    case doc of
        Empty ->
            Doc (initLCR string)

        Doc lcr ->
            Doc (appendAfterC string lcr)


insertChild : String -> Doc -> Doc
insertChild string doc =
    appendSibling string doc


viewDoc : Doc -> Html msg
viewDoc doc =
    div [ class "pa3" ]
        [ div [ class "f4 b pv2" ] [ text "Doc" ]
        , case doc of
            Empty ->
                div [] [ text "Doc Empty" ]

            Doc lcr ->
                div [] (viewLCR lcr)
        ]


viewSampleDoc : Html msg
viewSampleDoc =
    let
        sampleDoc : Doc
        sampleDoc =
            empty
                |> appendSibling "First"
                |> appendSibling "Second"
                |> appendSibling "Third"
                |> insertChild "Third's Child 1"
    in
    viewDoc sampleDoc
