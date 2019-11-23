module GOLGrid exposing (Grid, initDead, randomize)

import Dict exposing (Dict)
import Random exposing (Generator)


type alias Pos =
    ( Int, Int )


addPos : Pos -> Pos -> Pos
addPos ( x, y ) ( x2, y2 ) =
    ( x + x2, y + y2 )


type alias HasWH xx =
    { xx | w : Int, h : Int }


modPos : HasWH xx -> Pos -> Pos
modPos { w, h } ( x, y ) =
    ( modBy w x, modBy h y )


type Cell
    = Alive
    | Dead


type alias CellData =
    ( Cell, Int )


type alias Data =
    Dict Pos CellData


type alias Grid =
    { w : Int, h : Int, data : Data }


initDead : Int -> Int -> Grid
initDead w h =
    { w = w, h = h, data = Dict.empty }


randomize : Grid -> Generator Grid
randomize grid =
    randomDataGenerator grid |> Random.map (\data -> { grid | data = data })


randomDataGenerator : HasWH xx -> Generator Data
randomDataGenerator { w, h } =
    let
        maybePosCellGen : Pos -> Generator (Maybe ( Pos, Cell ))
        maybePosCellGen pos =
            Random.weighted ( 20, Just Alive ) [ ( 80, Nothing ) ]
                |> Random.map (Maybe.map (Tuple.pair pos))

        _ =
            Random.list (w * h)
    in
    Random.constant Dict.empty
