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


fromAlivePositions : List ( Int, Int ) -> Model
fromAlivePositions alivePositions =
    Debug.todo "impl"


setAliveAtAndIncrementNeighbours : Pos -> Model -> Model
setAliveAtAndIncrementNeighbours pos =
    let
        setAliveHelp : Maybe CellData -> CellData
        setAliveHelp =
            Maybe.map
                (\( _, anc ) -> ( Alive, anc ))
                >> Maybe.withDefault ( Alive, 0 )
    in
    Dict.update pos (setAliveHelp >> Just)
        >> incANC pos


neighbourOffsets : List ( Int, Int )
neighbourOffsets =
    [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ) ]
        ++ [ ( 0, -1 ), {- ignore self (0,0) , -} ( 0, 1 ) ]
        ++ [ ( 1, -1 ), ( 1, 0 ), ( 1, 1 ) ]


addPos ( x, y ) ( x2, y2 ) =
    ( x + x2, y + y2 )


modPos =
    identity


incNeighboursANC : Pos -> Model -> Model
incNeighboursANC pos model =
    let
        incHelp : Maybe CellData -> CellData
        incHelp =
            Maybe.map
                (\( cell, anc ) -> ( cell, anc + 1 ))
                >> Maybe.withDefault ( Dead, 1 )
    in
    List.foldl
        (addPos pos
            >> modPos
            >> (\nPos ->
                    Dict.update nPos (incHelp >> Just)
               )
        )
        model
        neighbourOffsets
