module GravitronV3.Draw exposing (Shape, rect, renderShape)

import GravitronV3.Transform as Transform exposing (Transform)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type Form
    = Rect Float Float


type Brush
    = Brush String


type Shape
    = Shape Form Brush Transform


rect : String -> Float -> Float -> Shape
rect color width height =
    Shape (Rect width height) (Brush color) Transform.initial


renderShape : Shape -> Svg msg
renderShape (Shape form (Brush fillColor) t) =
    let
        f =
            String.fromFloat
    in
    case form of
        Rect w h ->
            Svg.rect
                [ width <| f w
                , height <| f h
                , fill fillColor
                , transform <| Transform.renderRectTransform w h t
                ]
                []
