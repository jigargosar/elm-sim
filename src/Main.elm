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


type alias Board =
    { dict : Dict ( Int, Int ) Cell
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
    , start = { pos = ( 0, 0 ), dir = Left }
    }


getCellAt p board =
    if p == board.start.pos then
        Start board.start.dir

    else
        Dict.get p board.dict |> Maybe.withDefault Empty


boardCellList board =
    boardPositions
        |> List.map
            (\p ->
                ( p, getCellAt p board )
            )


renderBoardBackground cellWidth =
    let
        cellSize =
            ( cellWidth, cellWidth )

        renderCellBackground ( x, y ) =
            let
                isCenterCell =
                    x == 5 || x == 4

                bgColor =
                    if isCenterCell then
                        "#1f1f1f"

                    else
                        "#2e2e2e"
            in
            [ rect bgColor
                cellSize
                [ SA.rx (fromFloat (cellWidth / 15))
                , transformRect cellSize [ scale 0.985 ]
                ]
            ]
    in
    boardPositions
        |> List.map (\p -> ( p, renderCellBackground p ))
        |> gridLayout cellSize boardSize []


renderBoardLayer color cellWidth board =
    let
        { dict, start } =
            board

        cellSize =
            ( cellWidth, cellWidth )

        renderCell cell =
            case cell of
                Empty ->
                    []

                Start direction ->
                    [ circle color (cellWidth / 4) []
                    , words "white" "Start" [ transform [ scale (cellWidth / 16 / 6) ] ]
                    ]
    in
    boardCellList board
        |> List.map (\( p, c ) -> ( p, renderCell c ))
        |> gridLayout cellSize boardSize []


renderBoard cellWidth board =
    [ renderBoardBackground cellWidth
    , renderBoardLayer "dodgerblue" cellWidth board
        |> List.singleton
        |> group [ transform [ shift ( cellWidth / 5, cellWidth / 5 ) ] ]
    , renderBoardLayer "#d74d2e" cellWidth board
        |> List.singleton
        |> group [ transform [ shift ( -cellWidth / 5, -cellWidth / 5 ) ] ]
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
