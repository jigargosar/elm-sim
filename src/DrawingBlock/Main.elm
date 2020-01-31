module DrawingBlock.Main exposing (main)

import Browser
import Browser.Dom as BD
import Browser.Events as BE
import Html exposing (Html)
import String exposing (fromFloat)
import Svg as S
import Svg.Attributes as SA
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
    let
        ( w, h ) =
            ( model.width, model.height )
    in
    S.svg
        [ SA.viewBox ("0 0 " ++ fromFloat w ++ " " ++ fromFloat h)
        , SA.width "100%"
        , SA.height "100%"
        , SA.style
            """
                left : 0;
                top : 0;
                position : fixed;
            """
        ]
        [ S.rect
            [ floatAttribute SA.width (w / 2)
            , floatAttribute SA.height (h / 2)
            ]
            []
        ]


floatAttribute func float =
    func (String.fromFloat float)



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
