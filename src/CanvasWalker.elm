module CanvasWalker exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (..)
import Html.Attributes exposing (class)
import Task


type alias Model =
    { count : Float, width : Float, height : Float }


type Msg
    = Tick Float
    | GotViewport Viewport


main : Program () Model Msg
main =
    Browser.element
        { init =
            \() ->
                ( { count = 0
                  , width = 400
                  , height = 400
                  }
                , Task.perform GotViewport getViewport
                )
        , view = view
        , update =
            \msg model ->
                case msg of
                    Tick _ ->
                        ( { model | count = model.count + 1 }, Cmd.none )

                    GotViewport { viewport } ->
                        ( { model
                            | width = viewport.width
                            , height = viewport.height
                          }
                        , Cmd.none
                        )
        , subscriptions = \_ -> onAnimationFrameDelta Tick
        }


view : Model -> Html Msg
view { count, width, height } =
    div
        [ class "fullscreen-fixed content-centered"
        ]
        [ div [ class "debug" ] [ text "HW" ] ]
