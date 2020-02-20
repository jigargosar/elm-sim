module Doc1 exposing (viewSampleDoc)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import List.Extra
import String exposing (fromInt)


viewSampleDoc : Html msg
viewSampleDoc =
    viewDoc (parseDocString sampleStringDoc)


sampleStringDoc =
    """
Chapter 1
    C1:L1 Line 1
    C1:L2 Line 2
    C1:L3:S1 Section1
        C1:L3:S1 Line 1
        C1:L3:S1 Line 2
    C1:L4 Line 4
Chapter 2
    C2:L1 Line 1
    C2:L2 Line 2
    C2:L3:S1 Section1
        C2:L3:S1 Line 1
        C2:L3:S1 Line 2
    C2:L4 Line 4
Chapter 3 - Empty
Chapter 4 - Single Child Deep Nested
    Foo
        Bar
            Bazz
                Boom
                    Bang
    """


type Doc
    = Doc (List Line)


type Line
    = Line LineRecord


type alias LineRecord =
    { content : String, level : Int, isCollapsed : Bool }


parseDocString : String -> Doc
parseDocString string =
    let
        countWS s =
            String.split "    " s
                |> List.Extra.takeWhile String.isEmpty
                |> List.length
                |> Debug.log "l"

        toLine content =
            Line (LineRecord (String.trim content) (countWS content) False)

        lines =
            String.trim string
                |> String.lines
                |> List.map toLine
                |> Debug.log "all"
    in
    Doc lines


viewLine : Line -> Html msg
viewLine (Line lr) =
    div [ style "padding-left" (fromInt (lr.level * 20) ++ "px") ] [ text lr.content ]


viewDoc : Doc -> Html msg
viewDoc (Doc lines) =
    (div [ style "font-size" "2rem", class "pv10" ]
        [ text "Doc1 View" ]
        :: List.map viewLine lines
    )
        |> div [ class "df-col p20" ]
