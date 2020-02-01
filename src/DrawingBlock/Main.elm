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

        gridWH =
            ( 4, 3 )

        cellViewWH =
            ( cellWidth, cellWidth )

        gridViewWH =
            gridWH |> NT.toFloat |> NT.mul cellViewWH

        gridShift : Float2
        gridShift =
            NT.scale 0.5 cellViewWH
                |> NT.add (NT.scale -0.5 gridViewWH)

        gridIndexToGridCordinate : Int2 -> Float2
        gridIndexToGridCordinate idx =
            idx
                |> NT.toFloat
                |> NT.mul cellViewWH
                |> NT.add gridShift

        viewGridCell : ( Int2, Cell ) -> S.Svg msg
        viewGridCell ( idx, cell ) =
            renderCell cellWidth cell
                |> transformGridCell idx

        transformGridCell idx =
            wrapTransform [ shift (gridIndexToGridCordinate idx) ]

        grid =
            NT.toDict toCell gridWH
                |> Debug.log "foldl"

        viewGrid =
            Dict.toList grid
                |> List.map viewGridCell
                |> groupTransform []
      in
      viewGrid
    ]
        |> canvas ( model.width, model.height ) []


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
