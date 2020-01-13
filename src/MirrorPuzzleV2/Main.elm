module MirrorPuzzleV2.Main exposing (main)

import Basics.Extra exposing (inDegrees)
import Dict exposing (Dict)
import MirrorPuzzleV2.Grid as Grid
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
    | Empty


type alias CellDict =
    Dict Pos Cell


type alias Grid =
    Grid.Grid Cell


initMirror : Int -> Cell
initMirror =
    initDir >> Mirror


initialGrid : Grid
initialGrid =
    Grid.filled 5 5 Empty
        |> insert ( 1, 2 ) (SourceWithMirror (initDir 1))
        |> insert ( 2, 3 ) (initMirror 7)
        |> insert ( 3, 2 ) Destination
        |> insert ( 4, 4 ) (SourceWithMirror (initDir -3))
        |> insert ( 0, 0 ) Destination


insert : ( Int, Int ) -> Cell -> Grid -> Grid
insert =
    Grid.insert


computeLitDestinationPosSet : Grid -> Set Pos
computeLitDestinationPosSet grid =
    gridToLightPaths grid
        |> List.filterMap List.head
        |> List.foldl
            (\pos ->
                if Grid.get pos grid == Just Destination then
                    Set.insert pos

                else
                    identity
            )
            Set.empty


mapAllGridPositions : (Pos -> a) -> Grid -> List a
mapAllGridPositions func =
    Grid.map (\pos _ -> func pos) >> Grid.values


type alias LightPath =
    List Pos


gridToLightPaths : Grid -> List LightPath
gridToLightPaths grid =
    let
        accumLightPos : Direction -> Pos -> List Pos -> List Pos
        accumLightPos dir pos acc =
            let
                nextPos =
                    stepPosInDir dir pos
            in
            case Grid.get nextPos grid of
                Just (Mirror newMD) ->
                    accumLightPos newMD nextPos (nextPos :: acc)

                Just Destination ->
                    nextPos :: acc

                Nothing ->
                    acc

                _ ->
                    accumLightPos dir nextPos (nextPos :: acc)
    in
    Grid.foldl
        (\pos cell ->
            case cell of
                SourceWithMirror dir ->
                    accumLightPos dir pos [ pos ]
                        |> (::)

                _ ->
                    identity
        )
        []
        grid



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

        Empty ->
            noShape


pathToShape : Float -> List Pos -> Shape
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
                    Grid.viewDimensions cz grid
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
            renderShapeAt pos (toBgShape cz)

        lightPaths : List LightPath
        lightPaths =
            gridToLightPaths grid
    in
    group
        [ group
            [ mapAllGridPositions renderBgAt grid
                |> group
            , Grid.toList grid
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
