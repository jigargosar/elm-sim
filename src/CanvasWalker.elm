module CanvasWalker exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta)
import Class
import Html exposing (..)
import Html.Attributes exposing (class, classList, style)
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


shouldDebugLayout =
    False


hStack lst =
    Class.dFlex
        :: Class.fdRow
        :: Class.justifyCenter
        :: classList [ ( "layout-debug", shouldDebugLayout ) ]
        :: lst
        |> div


vStack lst =
    Class.dFlex
        :: Class.fdCol
        :: Class.justifyCenter
        :: classList [ ( "layout-debug", shouldDebugLayout ) ]
        :: lst
        |> div


viewCell =
    div
        [ style "width" "10px"
        , style "height" "10px"
        , style "background-color" "yellow"
        , style "outline" "2px solid rgba(0,0,0,1)"
        ]
        []


gridColumns =
    30


gridRows =
    gridColumns


viewGridRow =
    hStack [] (List.repeat gridColumns viewCell)


viewGrid =
    vStack []
        (List.repeat gridRows viewGridRow)


view : Model -> Html Msg
view { count, width, height } =
    vStack
        [ Class.pFixed
        , Class.trblZero
        ]
        [ viewGrid
        ]
