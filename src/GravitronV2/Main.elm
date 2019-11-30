module GravitronV2.Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import GravitronV2.Draw as Draw
import Html exposing (Html)
import Json.Decode as JD
import Task



-- Model


type alias Flags =
    ()


type alias Model =
    { mouse : Mouse
    , screen : Draw.Screen
    }


type alias Mouse =
    { x : Float
    , y : Float
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { mouse = Mouse 100 100
      , screen = Draw.screenFromWidthHeight 600 600
      }
    , Browser.Dom.getViewport |> Task.perform OnViewport
    )



-- Update


type Msg
    = Tick Float
    | MouseMoved Float Float
    | OnResize Int Int
    | OnViewport Browser.Dom.Viewport


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ Browser.Events.onAnimationFrameDelta Tick
    , JD.map2 MouseMoved
        (JD.field "pageX" JD.float)
        (JD.field "pageY" JD.float)
        |> Browser.Events.onMouseMove
    , Browser.Events.onResize OnResize
    ]
        |> Sub.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Tick _ ->
            ( onTick model
            , Cmd.none
            )

        MouseMoved mx my ->
            let
                mouse =
                    model.mouse

                screen =
                    model.screen
            in
            ( { model | mouse = { mouse | x = mx + screen.left, y = my + screen.top } }
            , Cmd.none
            )

        OnViewport { scene } ->
            ( { model | screen = Draw.screenFromWidthHeight scene.width scene.height }
            , Cmd.none
            )

        OnResize width height ->
            ( { model | screen = Draw.screenFromWidthHeight (toFloat width) (toFloat height) }
            , Cmd.none
            )


onTick : Model -> Model
onTick model =
    model



-- View


view : Model -> Html Msg
view model =
    let
        screen =
            model.screen

        x =
            screen.left

        y =
            screen.top

        w =
            screen.width

        h =
            screen.height
    in
    Draw.fullScreenCanvas x
        y
        w
        h
        Draw.black
        []



-- Program


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
