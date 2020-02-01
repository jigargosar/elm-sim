module DrawingBlock.Main exposing (main)

import Browser
import Browser.Dom as BD
import Browser.Events as BE
import Dict
import Dict2d
import DrawingBlock.Canvas exposing (..)
import Html exposing (Html)
import Number2 as NT exposing (Float2, Int2)
import PointFree exposing (is)
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
            ( 3, 3 )

        cellWH =
            ( cellWidth, cellWidth )

        gridViewWH =
            gridWH |> (NT.toFloat >> NT.scale cellWidth)

        cellView : Int2 -> S.Svg msg
        cellView idx =
            renderCell cellWidth
                |> transformCell idx cellWidth

        grid =
            NT.toDict (always ()) gridWH
                |> Debug.log "foldl"

        cellViews =
            Dict.keys grid
                |> List.map cellView
      in
      cellViews
        |> groupTransform [ shift (NT.sub gridViewWH cellWH |> NT.scale -0.5) ]
    ]
        |> canvas ( model.width, model.height ) []


transformCell : Int2 -> Float -> S.Svg msg -> S.Svg msg
transformCell cellIdx width =
    wrapTransform [ shift (cellIdx |> NT.toFloat |> NT.scale width) ]


renderCell : Float -> S.Svg msg
renderCell width =
    polySquare width [ fill "red" ]



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
