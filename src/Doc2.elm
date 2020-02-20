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


newChild : String -> Line -> Line
newChild string (Line { level }) =
    Line (LineRecord string (level + 1))


newRootLine : String -> Line
newRootLine string =
    Line (LineRecord string 0)


viewLine : Line -> Html msg
viewLine (Line l) =
    div [ levelPadding l.level ] [ text l.content ]


levelPadding : Int -> Html.Attribute msg
levelPadding level =
    style "padding-left" (fromInt (level * 20) ++ "px")


viewFocusedLine : Line -> Html msg
viewFocusedLine (Line l) =
    div
        [ levelPadding l.level
        , style "outline" "1px auto blue"
        ]
        [ text l.content ]



-- LCR


type alias LineZipper =
    Pivot Line


initLCR : String -> LineZipper
initLCR string =
    Pivot.singleton (newRootLine string)


appendSibling_ : String -> LineZipper -> LineZipper
appendSibling_ string p =
    Pivot.appendGoR (newSibling string (Pivot.getC p)) p


insertChild_ : String -> LineZipper -> LineZipper
insertChild_ string p =
    Pivot.appendGoR (newChild string (Pivot.getC p)) p


levelC : LineZipper -> Int
levelC =
    Pivot.getC >> (\(Line { level }) -> level)


gotoFirstChild : LineZipper -> Maybe LineZipper
gotoFirstChild parent =
    let
        isChild child =
            levelC parent + 1 == levelC child
    in
    Pivot.goR parent
        |> Maybe.Extra.filter isChild


gotoLastChild : LineZipper -> Maybe LineZipper
gotoLastChild p =
    gotoFirstChild p
        |> Maybe.map gotoLastSibling


gotoLastSibling : LineZipper -> LineZipper
gotoLastSibling p =
    case Pivot.goR p of
        Just p2 ->
            if levelC p2 == levelC p then
                gotoLastSibling p2

            else
                p

        Nothing ->
            p



--|> gotoLastSibling (Pivot.getC p)


viewLCR : LineZipper -> List (Html msg)
viewLCR p =
    Pivot.mapCS viewFocusedLine viewLine p |> Pivot.toList



-- DOC


type Doc
    = Doc LineZipper
    | Empty


empty : Doc
empty =
    Empty


init : String -> Doc
init string =
    Doc (initLCR string)


appendSibling : String -> Doc -> Doc
appendSibling string doc =
    case doc of
        Empty ->
            init string

        Doc lcr ->
            Doc (appendSibling_ string lcr)


appendChild : String -> Doc -> Doc
appendChild string doc =
    case doc of
        Empty ->
            init string

        Doc p ->
            case
                p
                    |> gotoLastChild
                    |> Maybe.map (appendSibling_ string)
            of
                Just p2 ->
                    Doc p2

                Nothing ->
                    insertChild_ string p
                        |> Doc


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
