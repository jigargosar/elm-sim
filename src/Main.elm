module Main exposing (main)

import Html exposing (div, text)
import Html.Attributes exposing (class)
import Set


type Shape
    = Square
    | HLine
    | VLine


allShapes =
    [ Square, HLine, VLine ]


allShapePoints : Set.Set ( Int, Int )
allShapePoints =
    List.range 0 3
        |> List.concatMap (\r -> List.range 0 3 |> List.map (Tuple.pair r))
        |> Set.fromList


shapeStringListToPoints list =
    list
        |> List.indexedMap
            (\r ->
                String.toList
                    >> List.indexedMap
                        (\c char ->
                            if char == '1' then
                                Just ( r, c )

                            else
                                Nothing
                        )
            )
        |> List.concat
        |> List.filterMap identity
        |> Set.fromList


shapeToString shape =
    case shape of
        Square ->
            [ "0000"
            , "0110"
            , "0110"
            , "0000"
            ]
                |> String.join "\n"

        HLine ->
            [ "0000"
            , "1111"
            , "0000"
            , "0000"
            ]
                |> String.join "\n"

        VLine ->
            [ "0100"
            , "0100"
            , "0100"
            , "0100"
            ]
                |> String.join "\n"


shapePoints shape =
    (case shape of
        Square ->
            [ "0000"
            , "0110"
            , "0110"
            , "0000"
            ]

        HLine ->
            [ "0000"
            , "1111"
            , "0000"
            , "0000"
            ]

        VLine ->
            [ "0100"
            , "0100"
            , "0100"
            , "0100"
            ]
    )
        |> shapeStringListToPoints


main =
    let
        viewShape shape =
            div []
                [ div [] [ text (Debug.toString shape) ]
                , div [] [ text " " ]
                , div [] [ text (shapeToString shape) ]
                , div [] [ text " " ]
                , div [] [ text (shapePoints shape |> Debug.toString) ]
                ]
    in
    div [ class "mono-board" ]
        [ div []
            (List.map viewShape allShapes
                |> List.intersperse (div [] [ text " " ])
            )
        ]
