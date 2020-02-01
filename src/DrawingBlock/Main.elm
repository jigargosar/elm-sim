module DrawingBlock.Main exposing (main)

import Browser
import Browser.Dom as BD
import Browser.Events as BE
import Dict
import DrawingBlock.Canvas exposing (..)
import Html exposing (Html)
import Number2 as NT exposing (Float2, Int2)
import String2 as ST
import Svg as S
import Task



-- Model


type alias Model =
    { width : Float
    , height : Float
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { width = 600
      , height = 600
      }
    , BD.getViewport |> Task.perform GotViewport
    )


setWidthHeight : Float -> Float -> Model -> Model
setWidthHeight width height model =
    { model | width = width, height = height }



-- Update


type Msg
    = GotViewport BD.Viewport
    | OnBrowserResize Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        GotViewport { scene } ->
            ( setWidthHeight scene.width scene.height model, Cmd.none )

        OnBrowserResize width height ->
            ( setWidthHeight (toFloat width) (toFloat height) model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ BE.onResize OnBrowserResize
    ]
        |> Sub.batch



-- View


view : Model -> Html Msg
view model =
    [ let
        ( w, h ) =
            ( model.width, model.height )
      in
      polyRect ( w / 2, h / 2 )
        [ fill "red"
        , transform [ scale 0.5, shift ( -w / 4, -h / 4 ) ]
        ]
        |> wrapTransform
            [ --
              shift ( -w / 4, -h / 4 )
            ]
    , let
        cellWidth =
            100

        gridD =
            ( 4, 3 )

        cellViewD =
            ( cellWidth, cellWidth )

        viewGridCell : ( Int2, Cell ) -> S.Svg msg
        viewGridCell ( idx, cell ) =
            renderCell cellWidth cell
                |> wrapTransform [ shift (gridIndexToGridCordinate (toGridContext cellViewD gridD) idx) ]

        grid =
            NT.toDict toCell gridD
                |> Debug.log "foldl"

        viewGrid =
            Dict.toList grid
                |> List.map viewGridCell
                |> groupTransform []
      in
      viewGrid
    ]
        |> canvas ( model.width, model.height ) []


type alias GridContext =
    { cellShift : Float2
    , cellViewD : Float2
    , gridD : Int2
    , gridViewD : Float2
    }


toGridContext : Float2 -> Int2 -> GridContext
toGridContext cellViewD gridD =
    let
        gridViewD =
            gridD |> NT.toFloat |> NT.mul cellViewD

        cellShift : Float2
        cellShift =
            NT.scale 0.5 cellViewD
                |> NT.add (NT.scale -0.5 gridViewD)
    in
    { cellShift = cellShift
    , cellViewD = cellViewD
    , gridD = gridD
    , gridViewD = gridViewD
    }


gridIndexToGridCordinate : GridContext -> Int2 -> Float2
gridIndexToGridCordinate { cellViewD, cellShift } idx =
    idx
        |> NT.toFloat
        |> NT.mul cellViewD
        |> NT.add cellShift


toCell ( x, y ) =
    Cell x y


type Cell
    = Cell Int Int


renderCell : Float -> Cell -> S.Svg msg
renderCell width (Cell x y) =
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



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
