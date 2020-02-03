module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Json.Decode as JD
import String exposing (fromInt)
import Svg as S exposing (svg, text, text_)
import Svg.Attributes as SA exposing (dominantBaseline, fill, textAnchor)
import Task
import Tuple exposing (mapBoth)
import TypedSvg.Attributes as TA exposing (viewBox)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


mapEach : (a -> x) -> ( a, a ) -> ( x, x )
mapEach f =
    mapBoth f f


type Puzzle
    = Puzzle (Grid Cell)


type Cell
    = Num Int
    | Empty


initPuzzle size =
    let
        toCell : Int -> Int -> Cell
        toCell x y =
            Num (x + 1 + (y * size))
    in
    gridInit size size toCell
        |> gridSet ( size - 1, size - 1 ) Empty
        |> Puzzle


resetPuzzle puzzle =
    initPuzzle (puzzleSize puzzle)


puzzleSize (Puzzle grid) =
    let
        ( size, _ ) =
            gridSize grid
    in
    size


puzzleGridSize (Puzzle grid) =
    gridSize grid


isPuzzleSolved puzzle =
    resetPuzzle puzzle == puzzle


swapEmptyInOppDir d ((Puzzle grid) as puzzle) =
    let
        maybeEmptyIdx =
            gridToList grid
                |> List.filter (\( _, v ) -> v == Empty)
                |> List.head
                |> Maybe.map Tuple.first
    in
    case maybeEmptyIdx of
        Just emptyIdx ->
            let
                swapIdx =
                    d4StepPos emptyIdx (d4Opposite d)
            in
            case gridGet swapIdx grid of
                Just swapCell ->
                    gridSet emptyIdx swapCell grid
                        |> gridSet swapIdx Empty
                        |> Puzzle

                Nothing ->
                    puzzle

        Nothing ->
            puzzle


type alias Model =
    { screenSize : ( Float, Float )
    , puzzle : Puzzle
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { screenSize = ( 600, 600 )
      , puzzle = initPuzzle 4
      }
    , Browser.Dom.getViewport |> Task.perform GotViewport
    )



-- Update


type Msg
    = OnResize Int Int
    | GotViewport Browser.Dom.Viewport
    | OnDirectionKeyDown Direction4
    | ResetPuzzle


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        OnResize w h ->
            ( { model | screenSize = ( toFloat w, toFloat h ) }, Cmd.none )

        GotViewport { scene } ->
            ( { model | screenSize = ( scene.width, scene.height ) }, Cmd.none )

        OnDirectionKeyDown direction4 ->
            ( { model | puzzle = swapEmptyInOppDir direction4 model.puzzle }, Cmd.none )

        ResetPuzzle ->
            ( { model | puzzle = resetPuzzle model.puzzle }, Cmd.none )


type Direction4
    = Up
    | Down
    | Left
    | Right


d4Opposite d =
    case d of
        Up ->
            Down

        Down ->
            Up

        Left ->
            Right

        Right ->
            Left


d4ToVec d =
    case d of
        Up ->
            ( 0, -1 )

        Down ->
            ( 0, 1 )

        Left ->
            ( -1, 0 )

        Right ->
            ( 1, 0 )


d4StepPos ( x, y ) d =
    let
        ( dx, dy ) =
            d4ToVec d
    in
    ( x + dx, y + dy )


onKeyDownSubscription =
    let
        directionKeyMap =
            [ ( "w", Up )
            , ( "s", Down )
            , ( "a", Left )
            , ( "d", Right )
            , ( "ArrowUp", Up )
            , ( "ArrowDown", Down )
            , ( "ArrowLeft", Left )
            , ( "ArrowRight", Right )
            ]
                |> List.map (Tuple.mapSecond OnDirectionKeyDown)

        keyMap =
            ( "r", ResetPuzzle )
                :: directionKeyMap
                |> Dict.fromList

        keyMsgDecoder key =
            case Dict.get key keyMap of
                Just msg ->
                    JD.succeed msg

                Nothing ->
                    JD.fail "nop"
    in
    JD.andThen keyMsgDecoder (JD.field "key" JD.string)


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ Browser.Events.onResize OnResize
    , Browser.Events.onKeyDown onKeyDownSubscription
    ]
        |> Sub.batch



-- view


getPuzzleCellWidth ( sw, sh ) (Puzzle grid) =
    let
        ( gridWidth, gridHeight ) =
            mapEach toFloat (gridSize grid)
    in
    min (sw * 0.9 / gridWidth) (sh * 0.9 / gridHeight)


view : Model -> Html Msg
view model =
    let
        cellWidth =
            getPuzzleCellWidth model.screenSize model.puzzle
    in
    canvas model.screenSize
        []
        [ viewPuzzle cellWidth model.puzzle
        ]


viewPuzzle : Float -> Puzzle -> S.Svg msg
viewPuzzle cellWidth ((Puzzle grid) as puzzle) =
    let
        bgColor =
            if isPuzzleSolved puzzle then
                "green"

            else
                "dodgerblue"
    in
    gridMap (renderCell bgColor cellWidth) grid
        |> gridToList
        |> gridLayout ( cellWidth, cellWidth ) (gridSize grid)


renderCell bgColor w _ cell =
    case cell of
        Num n ->
            [ square bgColor w []
            , words "black" (fromInt n) [ transform [ scale (w / 16 * 0.8) ] ]
            ]

        Empty ->
            []



-- GRID LIB


type Grid a
    = Grid ( Int, Int ) (Dict ( Int, Int ) a)


gridInit w h func =
    let
        foldXY x y =
            Dict.insert ( x, y ) (func x y)

        foldX x acc =
            List.foldl (foldXY x) acc (List.range 0 (h - 1))

        dict =
            List.foldl foldX Dict.empty (List.range 0 (w - 1))
    in
    Grid ( w, h ) dict


gridSet idx v ((Grid size dict) as grid) =
    if isValidGridIndex idx grid then
        Grid size (Dict.insert idx v dict)

    else
        grid


gridGet idx (Grid _ dict) =
    Dict.get idx dict


gridMap func (Grid size dict) =
    Grid size (Dict.map func dict)


gridToList (Grid _ dict) =
    Dict.toList dict


gridSize (Grid size _) =
    size


isValidGridIndex ( x, y ) (Grid ( w, h ) _) =
    x >= 0 && y >= 0 && x < w && y < h



-- Grid Layout


gridLayout cellSize gridSize_ =
    let
        transformCell ( idx, svgView ) =
            let
                ( x, y ) =
                    mapEach toFloat idx
            in
            svgView
                |> group [ transform [ shift ( x * cellWidth, y * cellHeight ) ] ]

        ( cellWidth, cellHeight ) =
            cellSize

        ( gridWidth, gridHeight ) =
            mapEach toFloat gridSize_

        dx =
            (cellWidth - (gridWidth * cellWidth)) / 2

        dy =
            (cellHeight - (gridHeight * cellHeight)) / 2
    in
    List.map transformCell
        >> group [ transform [ shift ( dx, dy ) ] ]



-- SVG CANVAS LIB


canvas ( w, h ) attrs =
    let
        ( x, y ) =
            ( -w / 2, -h / 2 )
    in
    svg
        (viewBox x y w h
            :: SA.shapeRendering "optimizeSpeed"
            :: HA.style "position" "fixed"
            :: HA.style "top" "0"
            :: HA.style "left" "0"
            :: HA.style "width" "100%"
            :: HA.style "height" "100%"
            :: attrs
        )


group =
    S.g


words color string attrs =
    text_
        (textAnchor "middle"
            :: dominantBaseline "central"
            :: fill color
            :: attrs
        )
        [ text string ]


square c w =
    rect c w w


rect color width height attrs =
    let
        ( x, y ) =
            ( width / 2, height / 2 )
    in
    S.polygon
        (TA.points [ ( -x, -y ), ( x, -y ), ( x, y ), ( -x, y ) ]
            :: fill color
            :: attrs
        )
        []


type alias Transform =
    { x : Float
    , y : Float
    , s : Float
    , deg : Float
    }


identityTransform =
    Transform 0 0 1 0


scale n t =
    { t | s = n }


shift ( dx, dy ) t =
    { t | x = t.x + dx, y = t.y + dy }


transform =
    List.foldl (<|) identityTransform
        >> transformToString
        >> SA.transform


transformToString { x, y, s, deg } =
    let
        t name args =
            String.concat
                [ name
                , "("
                , String.join " " (List.map String.fromFloat args)
                , ")"
                ]
    in
    t "translate" [ x, y ]
        ++ " "
        ++ t "scale" [ s ]
        ++ " "
        ++ t "rotate" [ deg ]
