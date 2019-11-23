module GOLDict exposing (..)

import Dict exposing (Dict)


type Cell
    = Alive
    | Dead


type alias CellData =
    ( Cell, Int )


type alias Pos =
    ( Int, Int )


type alias Model =
    Dict ( Int, Int ) CellData


fromAlivePositions : Int -> Int -> List Pos -> Model
fromAlivePositions width height =
    List.foldl (setAliveAtAndIncrementNeighbours width height) Dict.empty


setAliveAtAndIncrementNeighbours : Int -> Int -> Pos -> Model -> Model
setAliveAtAndIncrementNeighbours width height pos_ =
    let
        setAliveHelp : Maybe CellData -> CellData
        setAliveHelp =
            Maybe.map
                (\( _, anc ) -> ( Alive, anc ))
                >> Maybe.withDefault ( Alive, 0 )

        pos : Pos
        pos =
            modPos width height pos_
    in
    Dict.update pos (setAliveHelp >> Just)
        >> incNeighboursANC width height pos


neighbourOffsets : List ( Int, Int )
neighbourOffsets =
    [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ) ]
        ++ [ ( 0, -1 ), {- ignore self (0,0) , -} ( 0, 1 ) ]
        ++ [ ( 1, -1 ), ( 1, 0 ), ( 1, 1 ) ]


modPos : Int -> Int -> Pos -> Pos
modPos width height ( x, y ) =
    ( modBy width x, modBy height y )


incNeighboursANC : Int -> Int -> Pos -> Model -> Model
incNeighboursANC width height ( x, y ) model =
    let
        incHelp : Maybe CellData -> CellData
        incHelp =
            Maybe.map
                (\( cell, anc ) -> ( cell, anc + 1 ))
                >> Maybe.withDefault ( Dead, 1 )

        translateOffset ( dx, dy ) =
            modPos width height ( x + dx, y + dy )
    in
    List.foldl
        (translateOffset
            >> (\nPos ->
                    Dict.update nPos (incHelp >> Just)
               )
        )
        model
        neighbourOffsets
