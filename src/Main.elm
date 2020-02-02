module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html)
import Svg as S exposing (svg, text, text_)
import Svg.Attributes as SA exposing (dominantBaseline, fill, textAnchor)
import Task
import Tuple exposing (mapBoth)
import TypedSvg.Attributes as TA exposing (viewBox)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Int2 =
    ( Int, Int )


type alias Float2 =
    ( Float, Float )


mapEach : (a -> x) -> ( a, a ) -> ( x, x )
mapEach f =
    mapBoth f f


toFloat2 : Int2 -> Float2
toFloat2 =
    mapEach toFloat


type alias Model =
    { screenD : Float2
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { screenD = ( 600, 600 )
      }
    , Browser.Dom.getViewport |> Task.perform GotViewport
    )



-- Update


type Msg
    = OnResize Int Int
    | GotViewport Browser.Dom.Viewport


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        OnResize w h ->
            ( { model | screenD = ( w, h ) |> toFloat2 }, Cmd.none )

        GotViewport { scene } ->
            ( { model | screenD = ( scene.width, scene.height ) }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ Browser.Events.onResize OnResize ]
        |> Sub.batch



-- view


view : Model -> Html Msg
view model =
    let
        w =
            500
    in
    canvas model.screenD
        []
        [ square "dodgerblue" w []
        , words "black" "0" [ transform [ scale (w / 16) ] ]
        ]


canvas : Float2 -> List (S.Attribute a) -> List (S.Svg a) -> Html a
canvas ( w, h ) attrs =
    let
        ( x, y ) =
            ( -w / 2, -h / 2 )
    in
    svg (viewBox x y w h :: attrs)


words color string attrs =
    text_
        (textAnchor "middle"
            :: dominantBaseline "central"
            :: fill color
            :: attrs
        )
        [ text string ]


square c w =
    rect c w w


rect color width height attrs =
    let
        ( x, y ) =
            ( width / 2, height / 2 )
    in
    S.polygon
        (TA.points [ ( -x, -y ), ( x, -y ), ( x, y ), ( -x, y ) ]
            :: fill color
            :: attrs
        )
        []


type alias Transform =
    { x : Float
    , y : Float
    , s : Float
    , deg : Float
    }


identityTransform =
    Transform 0 0 1 0


scale n t =
    { t | s = n }


transform =
    List.foldl (<|) identityTransform
        >> transformToString
        >> SA.transform


transformToString : Transform -> String
transformToString { x, y, s, deg } =
    let
        t name args =
            String.concat
                [ name
                , "("
                , String.join " " (List.map String.fromFloat args)
                , ")"
                ]
    in
    t "translate" [ x, y ]
        ++ " "
        ++ t "scale" [ s ]
        ++ " "
        ++ t "rotate" [ deg ]
