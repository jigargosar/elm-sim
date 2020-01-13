module MirrorPuzzleV2.Main exposing (main)

import Basics.Extra exposing (inDegrees)
import Dict exposing (Dict)
import Playground exposing (..)
import PointFree exposing (mapEach, mulBy)
import Set exposing (Set)



-- Puzzle Data Model


type alias Pos =
    ( Int, Int )


type Direction
    = Direction Int


initDir : Int -> Direction
initDir ct =
    Direction (modBy 8 ct)


dirToDeg : Direction -> Float
dirToDeg (Direction ct) =
    45 * ct |> toFloat


stepPosInDir : Direction -> Pos -> Pos
stepPosInDir (Direction ct) ( x, y ) =
    let
        ( dx, dy ) =
            case ct of
                0 ->
                    ( 1, 0 )

                1 ->
                    ( 1, 1 )

                2 ->
                    ( 0, 1 )

                3 ->
                    ( -1, 1 )

                4 ->
                    ( -1, 0 )

                5 ->
                    ( -1, -1 )

                6 ->
                    ( 0, -1 )

                7 ->
                    ( 1, -1 )

                _ ->
                    ( 1, 0 )
    in
    ( x + dx, y + dy )


type Cell
    = Source
    | Destination
    | SourceWithMirror Direction
    | Mirror Direction


type Grid
    = Grid Int Int (Dict ( Int, Int ) Cell)


initMirror : Int -> Cell
initMirror =
    initDir >> Mirror


initialGrid : Grid
initialGrid =
    Grid 5 5 Dict.empty
        |> insert ( 1, 2 ) (SourceWithMirror (initDir 1))
        |> insert ( 2, 3 ) (initMirror 7)
        |> insert ( 3, 2 ) Destination
        |> insert ( 4, 4 ) (SourceWithMirror (initDir -3))
        |> insert ( 0, 0 ) Destination


insert : ( Int, Int ) -> Cell -> Grid -> Grid
insert pos cell ((Grid w h dict) as grid) =
    if isPositionInGrid pos grid then
        Dict.insert pos cell dict |> Grid w h

    else
        grid


isPositionInGrid : ( Int, Int ) -> Grid -> Bool
isPositionInGrid ( x, y ) (Grid w h _) =
    isValidIdx w x && isValidIdx h y


isValidIdx : number -> number -> Bool
isValidIdx len idx =
    idx >= 0 && idx < len


computeLitDestinationPosSet : Grid -> Set Pos
computeLitDestinationPosSet grid =
    let
        dict =
            gridToDict grid
    in
    gridToLightPaths grid
        |> List.filterMap List.head
        |> List.foldl
            (\pos ->
                if Dict.get pos dict == Just Destination then
                    Set.insert pos

                else
                    identity
            )
            Set.empty


gridToDict : Grid -> Dict ( Int, Int ) Cell
gridToDict (Grid _ _ dict) =
    dict


mapAllGridPositions : (( Int, Int ) -> a) -> Grid -> List a
mapAllGridPositions func (Grid w h _) =
    List.range 0 (h - 1)
        |> List.concatMap
            (\y ->
                List.range 0 (w - 1)
                    |> List.map (\x -> func ( x, y ))
            )


type alias LightPath =
    List Pos


gridToLightPaths : Grid -> List LightPath
gridToLightPaths grid =
    let
        dict =
            gridToDict grid
    in
    Dict.foldl
        (\pos cell ->
            case cell of
                SourceWithMirror md ->
                    Dict.insert pos md

                _ ->
                    identity
        )
        Dict.empty
        dict
        |> Dict.toList
        |> List.map (posDirToLightPath grid)


posDirToLightPath : Grid -> ( Pos, Direction ) -> LightPath
posDirToLightPath ((Grid _ _ dict) as grid) ( startPos, startDir ) =
    let
        collectWithStep dir pos acc =
            let
                nextPos =
                    stepPosInDir dir pos
            in
            if isPositionInGrid nextPos grid then
                case Dict.get nextPos dict of
                    Just (Mirror newMD) ->
                        collectWithStep newMD nextPos (nextPos :: acc)

                    Just Destination ->
                        nextPos :: acc

                    _ ->
                        collectWithStep dir nextPos (nextPos :: acc)

            else
                acc
    in
    collectWithStep startDir startPos [ startPos ]



-- Puzzle Grid View


toBgShape : Number -> Shape
toBgShape cz =
    group
        [ rectangle black cz cz |> scale 0.95
        , rectangle white cz cz |> scale 0.9
        ]


sourceShape : Number -> Shape
sourceShape cz =
    rectangle orange cz cz


mirrorShape : Number -> Direction -> Shape
mirrorShape cz dir =
    group
        [ group [ oval green (cz / 2) cz |> moveLeft (cz / 6) ]
            |> rotate (dirToDeg dir)
        , circle lightPurple 10
        ]


cellToShape : Number -> Cell -> Shape
cellToShape cz cell =
    case cell of
        Source ->
            sourceShape cz

        Mirror angDeg ->
            mirrorShape cz angDeg

        SourceWithMirror angDeg ->
            group
                [ sourceShape cz
                , mirrorShape cz angDeg
                ]

        Destination ->
            circle blue (cz / 2)


pathToShape cz =
    let
        cordsToShape : ( Float, Float ) -> ( Float, Float ) -> Shape
        cordsToShape p1 p2 =
            let
                ( x1, y1 ) =
                    p1

                ( x2, y2 ) =
                    p2

                ( dx, dy ) =
                    ( x2 - x1, y2 - y1 )

                len =
                    sqrt (dx ^ 2 + dy ^ 2)

                angRad =
                    atan2 dy dx

                angDeg =
                    inDegrees angRad
            in
            rectangle red len 5
                |> rotate angDeg
                |> move (x1 + dx / 2) (y1 + dy / 2)
    in
    List.map (mapEach (toFloat >> mulBy cz))
        >> (\path -> List.map2 cordsToShape path (List.drop 1 path))
        >> group


viewGrid : Time -> Grid -> Shape
viewGrid time grid =
    let
        cz =
            100

        ( dy, dx ) =
            let
                ( w, h ) =
                    let
                        (Grid iw ih _) =
                            grid
                    in
                    ( toFloat iw * cz, toFloat ih * cz )
            in
            ( (cz - w) / 2, (cz - h) / 2 )

        toViewPos =
            mapEach (toFloat >> mulBy cz)

        renderShapeAt =
            toViewPos >> (\( x, y ) -> move x y)

        blink =
            fade (zigzag 0.5 1 1 time)

        litDestinationsPosSet =
            computeLitDestinationPosSet grid

        renderCellAt ( pos, cell ) =
            cellToShape cz cell
                |> (if Set.member pos litDestinationsPosSet then
                        blink

                    else
                        identity
                   )
                |> scale 0.8
                |> renderShapeAt pos

        renderBgAt pos =
            renderShapeAt pos bgShape

        bgShape =
            toBgShape cz

        lightPaths : List LightPath
        lightPaths =
            gridToLightPaths grid
    in
    group
        [ group
            [ mapAllGridPositions renderBgAt grid
                |> group
            , gridToDict grid
                |> Dict.toList
                |> List.map renderCellAt
                |> group
            , lightPaths
                |> List.map (pathToShape cz)
                |> group
            ]
            |> move dx dy
        ]
        |> scale 1.5



-- Game Scaffold


type alias Mem =
    { grid : Grid }


init : Mem
init =
    { grid = initialGrid }


update : Computer -> Mem -> Mem
update _ mem =
    mem


view : Computer -> Mem -> List Shape
view { time } mem =
    [ viewGrid time mem.grid ]



--noinspection ElmUnusedSymbol


noShape =
    group []


main =
    game view update init
