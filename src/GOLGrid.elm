module GOLGrid exposing (Cell(..), Model, initDead, nextState, randomize, width)

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


type alias Model =
    { w : Int
    , h : Int
    , l : Int
    , data : Data
    , cords : List Pos
    }


width : Model -> Int
width =
    .w


initDead : Int -> Int -> Model
initDead w h =
    { w = w
    , h = h
    , l = w * h
    , data = Dict.empty
    , cords = toCords w h
    }


randomize : Model -> Generator Model
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

        maybeAlivePosGen : Pos -> Generator (Maybe Pos)
        maybeAlivePosGen pos =
            Random.weighted ( 10, Just Alive ) [ ( 90, Nothing ) ]
                |> Random.map (Maybe.map (always pos))

        posCellListGenerator : Generator (List Pos)
        posCellListGenerator =
            cords
                |> List.map maybeAlivePosGen
                |> Random.Extra.combine
                |> Random.map (List.filterMap identity)
    in
    posCellListGenerator |> Random.map (dataGeneratorFromAlivePosList gc)


neighbourOffsets : List ( Int, Int )
neighbourOffsets =
    [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ) ]
        ++ [ ( 0, -1 ), {- ignore self (0,0) , -} ( 0, 1 ) ]
        ++ [ ( 1, -1 ), ( 1, 0 ), ( 1, 1 ) ]


getValidNeighbourCords : HasWH xx -> Pos -> List Pos
getValidNeighbourCords hasWH pos =
    neighbourOffsets |> List.map (addPos pos >> modPos hasWH)


dataGeneratorFromAlivePosList : HasGridConfig xx -> List Pos -> Data
dataGeneratorFromAlivePosList gc =
    let
        setCellAlive : Pos -> Data -> Data
        setCellAlive pos =
            Dict.update pos
                (\maybeCellData ->
                    case maybeCellData of
                        Nothing ->
                            Just ( Alive, 0 )

                        Just ( _, ct ) ->
                            Just ( Alive, ct )
                )
                >> incAnc gc pos
    in
    List.foldl setCellAlive Dict.empty


incAnc : HasWH xx -> Pos -> Data -> Data
incAnc gc pos data =
    getValidNeighbourCords gc pos
        |> List.foldl
            (\nPos ->
                Dict.update nPos
                    (Maybe.withDefault ( Dead, 0 ) >> Tuple.mapSecond ((+) 1) >> Just)
            )
            data


decAnc : HasWH xx -> Pos -> Data -> Data
decAnc gc pos data =
    getValidNeighbourCords gc pos
        |> List.foldl
            (\nPos ->
                Dict.update nPos
                    (\maybeCellData ->
                        case maybeCellData of
                            Nothing ->
                                Debug.todo "invalid state"

                            Just ( Dead, 1 ) ->
                                Nothing

                            Just ( c, ct ) ->
                                if ct <= 0 then
                                    Debug.todo "invalid state"

                                else
                                    Just ( c, ct - 1 )
                    )
            )
            data


getNextCellData : CellData -> CellData
getNextCellData cellData =
    let
        ( cell, anc ) =
            cellData
    in
    case cell of
        Alive ->
            if anc < 2 || anc > 3 then
                ( Dead, anc )

            else
                cellData

        Dead ->
            if anc == 3 then
                ( Alive, anc )

            else
                cellData


nextState : Model -> Model
nextState grid =
    grid


nextState_ : Model -> Model
nextState_ grid =
    let
        getPrevCellData : Pos -> Maybe CellData
        getPrevCellData pos =
            Dict.get pos grid.data

        nextData =
            List.foldl
                (\pos ->
                    case getPrevCellData pos of
                        Nothing ->
                            identity

                        Just prevCellData ->
                            let
                                nextCellData =
                                    getNextCellData prevCellData
                            in
                            if prevCellData == nextCellData then
                                identity

                            else
                                Dict.insert pos nextCellData
                                    >> (case Tuple.first nextCellData of
                                            Alive ->
                                                incAnc grid pos

                                            Dead ->
                                                decAnc grid pos
                                       )
                )
                grid.data
                grid.cords
    in
    { grid | data = nextData }
