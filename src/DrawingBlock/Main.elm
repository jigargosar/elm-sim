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
        [ S.g [ SA.transform ("translate(" ++ fromFloat (w / 2) ++ "," ++ fromFloat (h / 2) ++ ")") ]
            [ rectangle (w / 2) (h / 2)
                |> toSvg
            ]
        ]


type Form
    = Polygon (List ( Float, Float ))
    | Ellipse Float Float


type Shape msg
    = Shape (ShapeAttributes msg) Form
    | CustomShape (S.Svg msg)
    | Group (ShapeAttributes msg) (List (Shape msg))


type alias ShapeAttributes msg =
    { x : Float
    , y : Float
    , scale : Float
    , degrees : Float
    , otherAttributes : List (S.Attribute msg)
    }


initialShapeAttributes : ShapeAttributes msg
initialShapeAttributes =
    ShapeAttributes 0 0 1 0 []


initShape : Form -> Shape msg
initShape =
    Shape initialShapeAttributes


rectangle : Float -> Float -> Shape msg
rectangle w h =
    let
        pt sx sy =
            ( w / 2 * sx, h / 2 * sy )
    in
    initShape (Polygon [ pt -1 -1, pt -1 1, pt 1 1, pt 1 -1 ])


toSvg : Shape msg -> S.Svg msg
toSvg shape =
    case shape of
        Shape shapeAttributes form ->
            case form of
                Polygon points ->
                    S.polygon
                        (SA.points (List.foldl addPoint "" points)
                            :: SA.transform (toTransformString shapeAttributes)
                            :: shapeAttributes.otherAttributes
                        )
                        []

                Ellipse w h ->
                    S.ellipse
                        (SA.rx (fromFloat w)
                            :: SA.ry (fromFloat h)
                            :: SA.transform (toTransformString shapeAttributes)
                            :: shapeAttributes.otherAttributes
                        )
                        []

        CustomShape svg ->
            svg

        Group shapeAttributes shapes ->
            S.g
                (SA.transform (toTransformString shapeAttributes)
                    :: shapeAttributes.otherAttributes
                )
                (List.map toSvg shapes)


addPoint : ( Float, Float ) -> String -> String
addPoint ( x, y ) str =
    str ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ " "


toTransformString : { a | x : Float, y : Float, scale : Float, degrees : Float } -> String
toTransformString shape =
    ("translate(" ++ fromFloat shape.x ++ "," ++ fromFloat shape.y ++ ")")
        ++ " "
        ++ ("scale(" ++ fromFloat shape.scale ++ ")")
        ++ " "
        ++ ("rotate(" ++ fromFloat shape.degrees ++ ")")



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
