module DrawingBlock.Main exposing (main)

import Basics.Extra exposing (uncurry)
import Browser
import Browser.Dom as BD
import Browser.Events as BE
import Dict exposing (Dict)
import DrawingBlock.Canvas exposing (..)
import DrawingBlock.Direction4 as D4
import Html exposing (Html)
import Json.Decode as JD exposing (Decoder)
import Number2 as NT exposing (Float2, Int2)
import PointFree exposing (dictSwap, ignoreNothing, is)
import String exposing (fromInt)
import String2 as ST
import Svg as S
import Task



-- Model


type alias Model =
    { canvasD : Float2
    , grid : Dict Int2 Cell
    , gridD : Int2
    }


toCell : Int -> Int2 -> Cell
toCell width ( x, y ) =
    let
        num =
            y * width + x + 1
    in
    if num == width * width then
        CellEmpty

    else
        CellNum num


type Cell
    = Cell Int Int
    | CellNum Int
    | CellEmpty


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        gridWidth =
            3

        gridD =
            NT.singleton gridWidth
    in
    ( { canvasD = ( 600, 600 )
      , grid = NT.toDict (toCell gridWidth) gridD
      , gridD = gridD
      }
    , BD.getViewport |> Task.perform GotViewport
    )


setCanvasD : Float2 -> Model -> Model
setCanvasD canvasD model =
    { model | canvasD = canvasD }



-- Update


type Msg
    = GotViewport BD.Viewport
    | OnBrowserResize Int Int
    | KeyDown D4.Label


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        GotViewport { scene } ->
            let
                canvasD =
                    ( scene.width, scene.height )
            in
            ( setCanvasD canvasD model, Cmd.none )

        OnBrowserResize width height ->
            let
                canvasD =
                    ( width, height ) |> NT.toFloat
            in
            ( setCanvasD canvasD model, Cmd.none )

        KeyDown d4 ->
            ( { model | grid = swapEmptyInDirection (D4.opposite d4) model.grid }
            , Cmd.none
            )


swapEmptyInDirection : D4.Label -> Dict Int2 Cell -> Dict Int2 Cell
swapEmptyInDirection d4 grid =
    case getEmptyPosition grid of
        Just emptyPosition ->
            let
                newPosition =
                    D4.step emptyPosition d4
            in
            (dictSwap emptyPosition newPosition |> ignoreNothing)
                grid

        Nothing ->
            grid


getEmptyPosition : Dict a Cell -> Maybe a
getEmptyPosition grid =
    Dict.toList grid
        |> List.filter (Tuple.second >> is CellEmpty)
        |> List.head
        |> Maybe.map Tuple.first


onKey : String.String -> a -> Decoder a
onKey key msg =
    JD.andThen
        (\actual ->
            if actual == key then
                JD.succeed msg

            else
                JD.fail ""
        )
        (JD.field "key" JD.string)


arrowKeyDecoder : Decoder D4.Label
arrowKeyDecoder =
    JD.oneOf
        [ onKey "ArrowUp" D4.Up
        , onKey "ArrowDown" D4.Down
        , onKey "ArrowLeft" D4.Left
        , onKey "ArrowRight" D4.Right
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ BE.onResize OnBrowserResize
    , arrowKeyDecoder
        |> JD.map KeyDown
        |> BE.onKeyDown
    ]
        |> Sub.batch



-- View


view : Model -> Html Msg
view model =
    [ let
        cellWidth =
            model.canvasD
                |> NT.scale 0.9
                |> NT.divBy (NT.toFloat model.gridD)
                |> uncurry min

        viewGridCell : Int2 -> Cell -> S.Svg msg
        viewGridCell idx cell =
            renderCell cellWidth cell
                |> wrapTransform [ shift (idx |> NT.toFloat |> NT.scale cellWidth) ]

        viewGrid =
            Dict.toList model.grid
                |> List.map (uncurry viewGridCell)
                |> groupTransform [ shift (computeGridShift cellWidth model.gridD) ]
      in
      viewGrid
    ]
        |> canvas model.canvasD []


renderCell : Float -> Cell -> S.Svg msg
renderCell width cell =
    case cell of
        Cell x y ->
            renderCellXY width x y

        CellNum num ->
            renderCellNum width num

        CellEmpty ->
            renderEmptyCell width


renderEmptyCell width =
    empty


renderCellNum width num =
    [ polySquare width [ fill "black" ]
    , words (fromInt num) [ fill "white", transform [ scale (width / 32) ] ]
    ]
        |> group []


renderCellXY : Float -> Int -> Int -> S.Svg msg
renderCellXY width x y =
    let
        cellColor =
            if x == 0 || y == 0 then
                "red"

            else
                "green"

        indexString =
            ST.fromInt ( x, y )
                |> ST.wrapJoin "(" "," ")"
    in
    [ polySquare width [ fill cellColor ]
    , words indexString [ fill "black" ]
    ]
        |> group []



-- Grid Transforms


computeGridShift : Float -> Int2 -> Float2
computeGridShift cellWidth gridD =
    let
        gridViewD =
            gridD |> NT.toFloat |> NT.scale cellWidth
    in
    NT.scale 0.5 ( cellWidth, cellWidth )
        |> NT.add (NT.scale -0.5 gridViewD)



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
