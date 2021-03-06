module NumberPuzzle.Main exposing (main)

import Basics.Extra exposing (uncurry)
import Browser
import Browser.Dom as BD
import Browser.Events as BE
import Dict exposing (Dict)
import Dict.Extra
import Html exposing (Html)
import Json.Decode as JD exposing (Decoder)
import List.Extra
import Number2 as NT exposing (Float2, Int2)
import NumberPuzzle.Canvas exposing (..)
import NumberPuzzle.Direction4 as D4 exposing (Label(..))
import PointFree exposing (dictSwap, ignoreNothing, is, shuffleValues)
import Random
import String exposing (fromInt)
import Svg as S
import Svg.Attributes as SA
import Task



-- Grid


type alias Grid =
    Dict Int2 Cell


type Cell
    = CellNum Int
    | CellEmpty


toCell : Int -> Int2 -> Cell
toCell width ( x, y ) =
    let
        num =
            y * width + x + 1
    in
    CellNum num


initGrid : Int2 -> Grid
initGrid (( w, _ ) as gridD) =
    NT.toDict (toCell w) gridD
        |> Dict.insert (NT.dec gridD) CellEmpty


shuffleGrid : Grid -> Random.Generator Grid
shuffleGrid =
    shuffleValues


swapEmptyInDirection : D4.Label -> Grid -> Grid
swapEmptyInDirection d4 grid =
    case getEmptyPosition grid of
        Just emptyIdx ->
            let
                newIdx =
                    D4.step emptyIdx d4
            in
            (dictSwap emptyIdx newIdx |> ignoreNothing)
                grid

        Nothing ->
            grid


swapWithEmptyNeighbourOf : Int2 -> Grid -> Grid
swapWithEmptyNeighbourOf ofIdx grid =
    case findEmptyNeighbourOf ofIdx grid of
        Just emptyNeighbourIdx ->
            (dictSwap ofIdx emptyNeighbourIdx |> ignoreNothing)
                grid

        Nothing ->
            grid


findEmptyNeighbourOf : Int2 -> Grid -> Maybe Int2
findEmptyNeighbourOf ofIdx grid =
    let
        neighgourIndices =
            [ Left, Right, Up, Down ] |> List.map (D4.step ofIdx)

        isEmpty idx =
            Dict.get idx grid == Just CellEmpty
    in
    neighgourIndices
        |> List.Extra.find isEmpty


getEmptyPosition : Grid -> Maybe Int2
getEmptyPosition =
    Dict.Extra.find (\_ -> is CellEmpty) >> Maybe.map Tuple.first


isSolved : Int2 -> Grid -> Bool
isSolved gridD grid =
    initGrid gridD == grid



-- Model


type alias Model =
    { canvasD : Float2
    , grid : Grid
    , gridD : Int2
    }


type alias Flags =
    { now : Int }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    let
        gridD =
            ( 4, 4 )

        initialGrid =
            initGrid gridD
    in
    ( { canvasD = ( 600, 600 )
      , grid = initialGrid
      , gridD = gridD
      }
    , [ BD.getViewport |> Task.perform GotViewport
      , getShuffledGrid initialGrid
      ]
        |> Cmd.batch
    )


getShuffledGrid : Grid -> Cmd Msg
getShuffledGrid grid =
    Random.generate GotShuffledGrid (shuffleGrid grid)


setCanvasD : Float2 -> Model -> Model
setCanvasD canvasD model =
    { model | canvasD = canvasD }



-- Update


type UserInput
    = DirectionKey D4.Label
    | ShuffleGrid
    | ResetGrid
    | Tap Float2


type Msg
    = GotViewport BD.Viewport
    | OnBrowserResize Int Int
    | GotUserInput UserInput
    | GotShuffledGrid Grid


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

        GotUserInput userInput ->
            case userInput of
                DirectionKey d4 ->
                    ( { model | grid = swapEmptyInDirection (D4.opposite d4) model.grid }
                    , Cmd.none
                    )

                ShuffleGrid ->
                    ( model, getShuffledGrid model.grid )

                ResetGrid ->
                    ( { model | grid = initGrid model.gridD }, Cmd.none )

                Tap pageXY ->
                    let
                        idx =
                            pageXYToGridIndex pageXY model.canvasD model.gridD
                    in
                    ( { model | grid = swapWithEmptyNeighbourOf idx model.grid }, Cmd.none )

        GotShuffledGrid grid ->
            ( { model | grid = grid }, Cmd.none )


pageXYToGridIndex pageXY canvasD gridD =
    let
        cellWidth =
            computeCellWidth canvasD gridD

        gridShift =
            computeGridShift cellWidth gridD
    in
    pageXYToCanvasXY pageXY canvasD
        |> NT.subBy gridShift
        |> NT.scale (1 / cellWidth)
        |> NT.round


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
    , JD.oneOf
        [ JD.map DirectionKey arrowKeyDecoder
        , onKey "s" ShuffleGrid
        , onKey "r" ResetGrid
        ]
        |> JD.map GotUserInput
        |> BE.onKeyDown
    , pageXYDecoder
        |> failUnlessPrimaryBtn
        |> JD.map (Tap >> GotUserInput)
        |> BE.onClick
    ]
        |> Sub.batch


pageXYDecoder : Decoder Float2
pageXYDecoder =
    JD.map2 Tuple.pair
        (JD.field "pageX" JD.float)
        (JD.field "pageY" JD.float)


failUnlessPrimaryBtn : Decoder a -> Decoder a
failUnlessPrimaryBtn d =
    JD.field "button" JD.int
        |> JD.andThen
            (\btn ->
                if btn == 0 then
                    d

                else
                    JD.fail ""
            )



-- View


computeCellWidth : Float2 -> Int2 -> Float
computeCellWidth canvasD gridD =
    canvasD
        |> NT.scale 0.9
        |> NT.divBy (NT.toFloat gridD)
        |> uncurry min


view : Model -> Html Msg
view model =
    [ let
        cellWidth =
            computeCellWidth model.canvasD model.gridD

        viewGridCell : Int2 -> Cell -> S.Svg msg
        viewGridCell idx cell =
            renderCell cellWidth cell
                |> wrap
                    [ fill
                        (if isSolved model.gridD model.grid then
                            "green"

                         else
                            "black"
                        )
                    , transform [ shift (idx |> NT.toFloat |> NT.scale cellWidth) ]
                    ]

        viewGrid =
            Dict.toList model.grid
                |> List.map (uncurry viewGridCell)
                |> groupTransform [ shift (computeGridShift cellWidth model.gridD) ]
      in
      viewGrid
    ]
        |> canvas model.canvasD
            [ --SA.shapeRendering "geometricPrecision"
              --, SA.shapeRendering "crispEdges"
              --,
              SA.shapeRendering "optimizeSpeed"
            ]


renderCell : Float -> Cell -> S.Svg msg
renderCell width cell =
    case cell of
        CellNum num ->
            renderCellNum width num

        CellEmpty ->
            empty


renderCellNum width num =
    [ polySquare width []
    , words (fromInt num) [ fill "white", transform [ scale (width / 32) ] ]
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
