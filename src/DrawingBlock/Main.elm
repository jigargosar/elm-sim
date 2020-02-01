module DrawingBlock.Main exposing (main)

import Basics.Extra exposing (uncurry)
import Browser
import Browser.Dom as BD
import Browser.Events as BE
import Dict exposing (Dict)
import DrawingBlock.Canvas exposing (..)
import Html exposing (Html)
import Number2 as NT exposing (Float2, Int2)
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


toCell : ( Int, Int ) -> Cell
toCell ( x, y ) =
    CellNum (x * y)


type Cell
    = Cell Int Int
    | CellNum Int
    | Empty


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        gridD =
            ( 4, 3 )
    in
    ( { canvasD = ( 600, 600 )
      , grid = NT.toDict toCell gridD
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ BE.onResize OnBrowserResize
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

        Empty ->
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
