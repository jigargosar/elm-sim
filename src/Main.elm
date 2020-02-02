module Main exposing (main)

import Dict exposing (Dict)
import Html as H exposing (Html, div)
import Html.Attributes exposing (class, style)


main : Html msg
main =
    let
        grid =
            initGrid 4
                |> swapEmptyInDirection Up
    in
    div [ flexCenter, fixedFullscreen ]
        [ renderGlobalStyles
        , renderGrid grid
        ]


type Grid
    = Grid Int (Dict ( Int, Int ) Cell)


type Cell
    = Num Int
    | Empty


initGrid : Int -> Grid
initGrid size =
    let
        dict =
            times size (\r -> times size (Tuple.pair r))
                |> List.concat
                |> List.indexedMap (\i rc -> ( rc, Num (i + 1) ))
                |> Dict.fromList
    in
    Dict.insert ( size - 1, size - 1 ) Empty dict
        |> Grid size


gridToRows : Grid -> List (List Cell)
gridToRows (Grid size dict) =
    let
        getRow n =
            dict
                |> Dict.filter (\( rowIdx, _ ) _ -> rowIdx == n)
                |> Dict.values
    in
    times size getRow


swapEmptyInDirection : Direction -> Grid -> Grid
swapEmptyInDirection direction =
    swapEmptyWith (nextPositionInDirection direction)


swapEmptyWith nextPosFunc ((Grid size dict) as grid) =
    let
        getEmptyPosition =
            Dict.filter (\_ cell -> cell == Empty) dict
                |> Dict.keys
                |> List.head
    in
    case getEmptyPosition of
        Just emptyPos ->
            let
                nextPos =
                    nextPosFunc emptyPos
            in
            case Dict.get nextPos dict of
                Just nextCell ->
                    dict
                        |> Dict.insert nextPos Empty
                        |> Dict.insert emptyPos nextCell
                        |> Grid size

                Nothing ->
                    grid

        Nothing ->
            grid


dec =
    (+) -1


inc =
    (+) 1


type Direction
    = Up
    | Down
    | Left
    | Right


nextPositionInDirection : Direction -> ( number, number ) -> ( number, number )
nextPositionInDirection direction =
    case direction of
        Up ->
            Tuple.mapFirst dec

        Down ->
            Tuple.mapFirst inc

        Left ->
            Tuple.mapSecond dec

        Right ->
            Tuple.mapSecond inc


renderGrid : Grid -> Html msg
renderGrid grid =
    let
        renderRow =
            List.map renderCell
                >> div [ flex ]
    in
    gridToRows grid
        |> List.map renderRow
        |> div [ flex, flexColumn, cellBorder ]


renderCell : Cell -> Html msg
renderCell cell =
    case cell of
        Num num ->
            renderCellString (String.fromInt num)
                [ style "background-color" "gray"
                ]

        Empty ->
            renderCellString "" []


renderCellString cellContent attrs =
    div
        ([ style "width" "200px"
         , style "height" "200px"
         , cellBorder
         , style "font-size" "80px"
         , style "font-family" "monospace"
         , flexCenter
         ]
            ++ attrs
        )
        [ H.text cellContent ]


cellBorder =
    style "border" "1px solid black"


times n func =
    List.range 0 (n - 1) |> List.map func



-- STYLES


renderGlobalStyles =
    H.node "style" [] [ H.text globalStyles ]


globalStyles =
    """
        .flex-center{
            display: flex;
            align-items: center;
            justify-content: center;
        }

        .fixed-fullscreen {
            position: fixed;
            width: 100%;
            height: 100%;
            top: 0;
            left: 0;
        }
    """


flexCenter =
    class "flex-center"


fixedFullscreen =
    class "fixed-fullscreen"


flex =
    style "display" "flex"


flexColumn =
    style "flex-direction" "column"
