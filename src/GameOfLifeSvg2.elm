module GameOfLifeSvg2 exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events
import Color
import Html.Attributes as HA
import Random exposing (Generator, Seed)
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Types as ST


type alias Flags =
    { now : Int }


type alias Model =
    { grid : Grid
    , seed : Seed
    }


type alias Grid =
    { width : Int
    , height : Int
    , data : Array Cell
    }


type Cell
    = Alive
    | Dead


initialGrid : Grid
initialGrid =
    let
        width =
            30

        height =
            width
    in
    Array.repeat (width * height) Dead
        |> Grid width height


init : Flags -> ( Model, Cmd Msg )
init { now } =
    let
        model =
            { grid = initialGrid
            , seed = Random.initialSeed now
            }
    in
    ( model |> randomizeGrid
    , Cmd.none
    )


gridGenerator : Int -> Int -> Generator Grid
gridGenerator width height =
    Random.list (width * height) (Random.weighted ( 20, Alive ) [ ( 80, Dead ) ])
        |> Random.map (Array.fromList >> Grid width height)


setGridFromTuple : ( Grid, Model ) -> Model
setGridFromTuple ( grid, model ) =
    { model | grid = grid }


randomizeGrid : Model -> Model
randomizeGrid model =
    let
        { width, height } =
            model.grid
    in
    model
        |> randomStep (gridGenerator width height)
        |> setGridFromTuple


randomStep : Generator a -> Model -> ( a, Model )
randomStep generator model =
    Random.step generator model.seed
        |> Tuple.mapSecond (\seed -> { model | seed = seed })


type Msg
    = Tick Float


update : Msg -> Model -> ( Model, Cmd msg )
update message model =
    case message of
        Tick delta ->
            ( updateGridOnTick model, Cmd.none )


updateGridOnTick : Model -> Model
updateGridOnTick model =
    model


view model =
    let
        w =
            600

        h =
            w

        gridWidthInPx =
            w - 2

        grid =
            model.grid

        cellWidthInPx =
            toFloat gridWidthInPx / toFloat grid.width

        viewCell ( gridX, gridY ) cell =
            let
                x =
                    toFloat gridX * cellWidthInPx + 1

                y =
                    toFloat gridY * cellWidthInPx + 1
            in
            S.rect
                [ (if cell == Dead then
                    Color.lightYellow

                   else
                    Color.lightRed
                  )
                    |> ST.Fill
                    |> SA.fill
                , SA.x (ST.px x)
                , SA.y (ST.px y)
                , SA.width (ST.px cellWidthInPx)
                , SA.height (ST.px cellWidthInPx)
                ]
                []
    in
    S.svg [ SA.viewBox 0 0 w h, HA.width w ]
        [ S.g
            [ SA.stroke Color.black
            , SA.strokeWidth (ST.px 2)
            ]
            (grid.data
                |> Array.indexedMap (\i -> viewCell (gridIndexToXY i grid))
                |> Array.toList
            )
        ]


gridIndexToXY : Int -> Grid -> ( Int, Int )
gridIndexToXY i grid =
    let
        x =
            remainderBy grid.width i

        y =
            i // grid.height
    in
    ( x, y )


xyToGridIndex : Int -> Int -> Grid -> Int
xyToGridIndex x y grid =
    modBy grid.width x + modBy grid.height y * grid.height


main =
    Browser.element
        { init = init
        , subscriptions = \_ -> Browser.Events.onAnimationFrameDelta Tick
        , update = update
        , view = view
        }
