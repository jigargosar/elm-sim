module Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import String exposing (fromFloat)
import Svg as S exposing (svg, text, text_)
import Svg.Attributes as SA exposing (dominantBaseline, fill, stroke, textAnchor)
import Task
import Tuple exposing (mapBoth)
import TypedSvg.Attributes as TA exposing (viewBox)
import TypedSvg.Types as T


type Cell
    = Empty
    | Start Direction


type Direction
    = Up
    | Down
    | Left
    | Right


dirToDeg dir =
    case dir of
        Up ->
            -90

        Down ->
            90

        Left ->
            180

        Right ->
            0


dirToUnitVec dir =
    case dir of
        Up ->
            ( 0, -1 )

        Down ->
            ( 0, 1 )

        Left ->
            ( -1, 0 )

        Right ->
            ( 1, 0 )


type alias Board =
    { dict : Dict ( Int, Int ) Cell
    , move : Dict ( Int, Int ) Direction
    , start : { pos : ( Int, Int ), dir : Direction }
    }


boardWidth =
    10


boardHeight =
    8


boardSize =
    ( boardWidth, boardHeight )


boardPositions =
    List.range 0 (boardWidth - 1)
        |> List.concatMap (\x -> List.range 0 (boardHeight - 1) |> List.map (Tuple.pair x))


emptyBoard : Board
emptyBoard =
    { dict = Dict.empty
    , move =
        Dict.empty

    --|> Dict.insert ( 2, 0 ) Down
    --|> Dict.insert ( 2, 1 ) Up
    --|> Dict.insert ( 2, 2 ) Left
    --|> Dict.insert ( 1, 2 ) Right
    , start = { pos = ( 4, 1 ), dir = Left }
    }


getCellAt p board =
    if p == board.start.pos then
        Start board.start.dir

    else
        Dict.get p board.dict |> Maybe.withDefault Empty


getMoveAt p board =
    Dict.get p board.move


boardCellList board =
    boardPositions
        |> List.map
            (\p ->
                ( p, getCellAt p board )
            )


boardMoveList board =
    boardPositions
        |> List.filterMap
            (\p ->
                getMoveAt p board
                    |> Maybe.map (Tuple.pair p)
            )


type Background
    = Light
    | Darker
    | Dark


classifyBackground ( x, y ) =
    if x == 4 || x == 5 then
        -- Center
        Dark

    else if (x < 4 && y < 4) || (x > 4 && y > 3) then
        -- Quad 1
        Light

    else
        -- Quad 2
        Darker


renderCellBackgroundTile cellSize cellWidth p =
    let
        bgColor =
            case classifyBackground p of
                Light ->
                    "#383838"

                Dark ->
                    "#2e2e2e"

                Darker ->
                    "#242424"
    in
    [ rect bgColor
        cellSize
        [ SA.rx (fromFloat (cellWidth / 15))
        , transformRect cellSize [ scale 0.985 ]
        ]
    ]


renderBoardBackgroundTileLayer cellWidth =
    let
        cellSize =
            ( cellWidth, cellWidth )
    in
    boardPositions
        |> List.map (\p -> ( p, renderCellBackgroundTile cellSize cellWidth p ))
        |> gridLayout cellSize boardSize []


renderCell color cellWidth cell =
    let
        radius =
            cellWidth / 6
    in
    case cell of
        Empty ->
            []

        Start direction ->
            [ empty
            , triangle color
                radius
                [ stroke "white"
                , transform
                    [ rotate (dirToDeg direction + 90)
                    , shift (dirToUnitVec direction |> mapEach (mul (radius * 0.5)))
                    ]
                ]
            , circle color
                radius
                [ stroke "white"

                --, transform [ shift ( cellWidth / 30, 0 ) ]
                ]
            , words "white" "Start" [ transform [ scale (radius / 16 * 0.75) ] ]
            ]


renderMove color offset cellWidth direction =
    let
        radius =
            cellWidth / 6
    in
    [ triangle color
        radius
        [ stroke "white"
        , transform
            [ rotate (dirToDeg direction + 90)
            , shift (dirToUnitVec direction |> mapBoth (mul (cellWidth / 3)) (mul (cellWidth / 3)))
            , case direction of
                Up ->
                    shift ( offset, 0 )

                Down ->
                    shift ( offset, 0 )

                Left ->
                    shift ( 0, offset )

                Right ->
                    shift ( 0, offset )
            ]
        ]
    ]


swap ( a, b ) =
    ( b, a )


renderMovementLayer color offset cellWidth board =
    let
        cellSize =
            ( cellWidth, cellWidth )
    in
    boardMoveList board
        |> List.map (\( p, v ) -> ( p, renderMove color offset cellWidth v ))
        |> gridLayout cellSize boardSize []


renderInstructionLayer color cellWidth board =
    let
        { dict, start } =
            board

        cellSize =
            ( cellWidth, cellWidth )
    in
    boardCellList board
        |> List.map (\( p, c ) -> ( p, renderCell color cellWidth c ))
        |> gridLayout cellSize boardSize []


mul =
    (*)


renderBoard cellWidth board =
    let
        shiftLayer factor =
            List.singleton
                >> group [ transform [ shift ( factor, factor ) ] ]
    in
    [ renderBoardBackgroundTileLayer cellWidth
    , renderInstructionLayer "#1e90ff" cellWidth board
        |> shiftLayer (cellWidth / 5)
    , renderInstructionLayer "#d74d2e" cellWidth board
        |> shiftLayer (-cellWidth / 5)
    , renderMovementLayer "#1e90ff" (-cellWidth / 5) cellWidth board
    , renderMovementLayer "#d74d2e" (cellWidth / 5) cellWidth board
    ]
        |> group []



-- Model


type alias Model =
    { screenSize : ( Float, Float ) }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        size =
            ( 600, 600 )
    in
    ( { screenSize = size }
    , Browser.Dom.getViewport |> Task.perform GotViewport
    )



-- Update


type Msg
    = NoOp
    | GotViewport Browser.Dom.Viewport
    | OnResize Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        GotViewport { scene } ->
            ( { model | screenSize = ( scene.width, scene.height ) }, Cmd.none )

        OnResize _ _ ->
            ( model, Browser.Dom.getViewport |> Task.perform GotViewport )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize OnResize
        ]



-- View


view : Model -> Html Msg
view model =
    let
        ( sw, sh ) =
            model.screenSize

        cellWidth =
            min (sw / boardWidth) (sh / boardHeight)
    in
    canvas model.screenSize
        [ HA.style "background-color" "black" ]
        [ polyRect "dodgerblue" ( 100, 100 ) []
        , renderBoard cellWidth emptyBoard
        ]



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Grid Layout


mapEach : (a -> x) -> ( a, a ) -> ( x, x )
mapEach f =
    mapBoth f f


gridLayout cellSize gridSize_ attrs =
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
        >> group (transform [ shift ( dx, dy ) ] :: attrs)



-- SVG CANVAS LIB


canvas ( w, h ) attrs =
    let
        ( x, y ) =
            ( -w / 2, -h / 2 )
    in
    svg
        (viewBox x y w h
            :: geometricPrecision
            :: TA.imageRendering T.RenderingOptimizeQuality
            :: TA.textRendering T.TextRenderingOptimizeLegibility
            :: HA.style "position" "fixed"
            :: HA.style "top" "0"
            :: HA.style "left" "0"
            :: HA.style "width" "100%"
            :: HA.style "height" "100%"
            :: attrs
        )


geometricPrecision =
    TA.shapeRendering T.RenderGeometricPrecision


crispEdges =
    TA.shapeRendering T.RenderCrispEdges


strokeWidth =
    fromFloat >> SA.strokeWidth


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
    polyRect c ( w, w )


polyRect color ( width, height ) attrs =
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


polygon color points attrs =
    S.polygon
        (TA.points points
            :: fill color
            :: attrs
        )
        []


rect color ( width, height ) attrs =
    let
        ( x, y ) =
            ( width / 2, height / 2 )
    in
    S.rect
        (SA.width (fromFloat width)
            :: SA.height (fromFloat height)
            :: fill color
            :: attrs
        )
        []


circle color r =
    ellipse color ( r, r )


ellipse color ( width, height ) attrs =
    S.ellipse
        (SA.rx (fromFloat width)
            :: SA.ry (fromFloat height)
            :: TA.shapeRendering T.RenderGeometricPrecision
            :: fill color
            :: attrs
        )
        []


triangle color radius =
    ngon color 3 radius


ngon color sides radius attrs =
    S.polygon
        (SA.points (toNgonPoints 0 sides radius "")
            :: fill color
            :: attrs
        )
        []


toNgonPoints : Int -> Int -> Float -> String -> String
toNgonPoints i n radius string =
    if i == n then
        string

    else
        let
            a =
                turns (toFloat i / toFloat n - 0.25)

            x =
                radius * cos a

            y =
                radius * sin a
        in
        toNgonPoints (i + 1) n radius (string ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ " ")


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


rotate deg t =
    { t | deg = deg }


shift ( dx, dy ) t =
    { t | x = t.x + dx, y = t.y + dy }


transform =
    List.foldl (<|) identityTransform
        >> transformToString
        >> SA.transform


transformRect ( w, h ) =
    List.foldl (<|) identityTransform
        >> shift ( -w / 2, -h / 2 )
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


fade =
    fromFloat >> SA.opacity


empty =
    S.text ""
