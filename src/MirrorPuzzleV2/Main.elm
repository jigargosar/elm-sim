module MirrorPuzzleV2.Main exposing (main)

import Basics.Extra exposing (inDegrees)
import Dict exposing (Dict)
import Playground exposing (..)
import PointFree exposing (mapEach, mulBy)



-- Puzzle Data Model


type alias Pos =
    ( Int, Int )


type Direction
    = Direction Int


initDir : Int -> Direction
initDir ct =
    Direction (remainderBy 8 ct)


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
      --| Destination
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


gridToMaybeCellList : Grid -> List ( ( Int, Int ), Maybe Cell )
gridToMaybeCellList (Grid w h dict) =
    List.range 0 (h - 1)
        |> List.concatMap
            (\y ->
                List.range 0 (w - 1)
                    |> List.map (\x -> ( ( x, y ), Dict.get ( x, y ) dict ))
            )



-- Puzzle Grid View


bgShape : Number -> Shape
bgShape cz =
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
    group
        [ bgShape cz
        , (case cell of
            Source ->
                sourceShape cz

            Mirror angDeg ->
                mirrorShape cz angDeg

            SourceWithMirror angDeg ->
                group
                    [ sourceShape cz
                    , mirrorShape cz angDeg
                    ]
          )
            |> scale 0.8
        ]


viewMaybeCell : Number -> ( ( Int, Int ), Maybe Cell ) -> Shape
viewMaybeCell cz ( ( x, y ), maybeCell ) =
    maybeCell
        |> Maybe.map (cellToShape cz)
        |> Maybe.withDefault (bgShape cz)
        |> move (toFloat x * cz) (toFloat y * cz)


pathCordsToShape : ( Float, Float ) -> ( Float, Float ) -> Shape
pathCordsToShape p1 p2 =
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


viewGrid : Grid -> Shape
viewGrid grid =
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

        viewLightPath path =
            List.map2 pathCordsToShape path (List.drop 1 path)
                |> group
    in
    group
        [ gridToMaybeCellList grid
            |> List.map (viewMaybeCell cz)
            |> group
            |> move dx dy
        , gridToLightPath grid
            |> List.map (mapEach (toFloat >> mulBy cz))
            |> viewLightPath
            |> move dx dy
        ]
        |> scale 1.5


gridToLightPath ((Grid _ _ dict) as grid) =
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
        |> List.head
        |> Maybe.map (posDirToLightPath grid)
        |> Maybe.withDefault []


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
                        collectWithStep newMD nextPos (pos :: acc)

                    _ ->
                        collectWithStep dir nextPos (pos :: acc)

            else
                pos :: acc
    in
    collectWithStep startDir startPos []



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
view _ mem =
    [ viewGrid mem.grid ]



--noinspection ElmUnusedSymbol


noShape =
    group []


main =
    game view update init
