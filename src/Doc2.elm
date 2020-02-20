module Doc2 exposing (viewSampleDoc)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Maybe.Extra
import Pivot exposing (Pivot)
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


viewFocusedLine : Line -> Html msg
viewFocusedLine (Line l) =
    div
        [ style "padding-left" (fromInt l.level ++ "px")
        , style "outline" "1px auto blue"
        ]
        [ text l.content ]



-- LCR


type alias LCR =
    Pivot Line


initLCR : String -> LCR
initLCR string =
    Pivot.singleton (newRootLine string)


appendAfterC : String -> LCR -> LCR
appendAfterC string p =
    Pivot.appendGoR (newSibling string (Pivot.getC p)) p


viewLCR : LCR -> List (Html msg)
viewLCR p =
    Pivot.mapCS viewFocusedLine viewLine p |> Pivot.toList



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


appendChild : String -> Doc -> Doc
appendChild string doc =
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
                |> appendChild "Third's Child 1"
    in
    viewDoc sampleDoc
