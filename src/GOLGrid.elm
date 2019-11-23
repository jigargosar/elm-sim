module GOLGrid exposing (Grid, initDead, randomize)

import Dict exposing (Dict)
import Random exposing (Generator)
import Random.Extra


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
    { w : Int
    , h : Int
    , data : Data
    , cords : List Pos
    }


initDead : Int -> Int -> Grid
initDead w h =
    { w = w
    , h = h
    , data = Dict.empty
    , cords = toCords w h
    }


randomize : Grid -> Generator Grid
randomize grid =
    randomDataGenerator grid |> Random.map (\data -> { grid | data = data })


toCords : Int -> Int -> List Pos
toCords w h =
    let
        widthRange =
            List.range 0 (w - 1)

        heightRange =
            List.range 0 (h - 1)
    in
    List.concatMap (\y -> List.map (\x -> ( x, y )) widthRange) heightRange


type alias HasGridConfig xx =
    { xx | w : Int, h : Int, l : Int, cords : List Pos }


randomDataGenerator : HasGridConfig xx -> Generator Data
randomDataGenerator gc =
    let
        { l, cords } =
            gc

        maybePosCellGen : Pos -> Generator (Maybe ( Pos, Cell ))
        maybePosCellGen pos =
            Random.weighted ( 20, Just Alive ) [ ( 80, Nothing ) ]
                |> Random.map (Maybe.map (Tuple.pair pos))

        posCellListGenerator : Generator (List ( Pos, Cell ))
        posCellListGenerator =
            cords
                |> List.map maybePosCellGen
                |> Random.Extra.combine
                |> Random.map (List.filterMap identity)
    in
    posCellListGenerator |> Random.map (dataGeneratorFromPosCellList gc)


dataGeneratorFromPosCellList : HasGridConfig xx -> List ( Pos, Cell ) -> Data
dataGeneratorFromPosCellList gc =
    always Dict.empty
