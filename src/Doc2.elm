module Doc2 exposing (viewSampleDoc)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import String exposing (fromInt)



-- Line


type Line
    = Line LineRecord


type alias LineRecord =
    { content : String
    , level : Int
    }


newSibling : String -> Line -> Line
newSibling string (Line { level }) =
    Line (LineRecord string level)


newRootLine : String -> Line
newRootLine string =
    Line (LineRecord string 0)


viewLine : Line -> Html msg
viewLine (Line l) =
    div [ style "padding-left" (fromInt l.level ++ "px") ] [ text l.content ]



-- LCR


type LCR
    = LCR ( List Line, Line, List Line )


initLCR : String -> LCR
initLCR string =
    LCR ( [], newRootLine string, [] )


appendAfterC : String -> LCR -> LCR
appendAfterC string (LCR ( l, c, r )) =
    LCR ( c :: l, newSibling string c, r )


toList : LCR -> List Line
toList (LCR ( l, c, r )) =
    List.reverse l ++ c :: r


viewLCR : LCR -> List (Html msg)
viewLCR lcr =
    toList lcr |> List.map viewLine



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
