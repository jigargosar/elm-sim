module Main exposing (main)

import Html.Attributes as HA
import String exposing (fromFloat)
import Svg as S exposing (svg, text, text_)
import Svg.Attributes as SA exposing (dominantBaseline, fill, textAnchor)
import TypedSvg.Attributes as TA exposing (viewBox)


type alias NodeAttributes =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , fill : String
    }


type Node
    = Layer NodeAttributes (List Node)
    | Node NodeAttributes


newLayer : List Node -> Node
newLayer =
    Layer (NodeAttributes 0 0 0 0 "")


newRect x y w h =
    NodeAttributes x y w h "red" |> Node


drawNode node =
    case node of
        Node shape ->
            let
                { x, y, width, height } =
                    shape
            in
            rect shape.fill width height [ transform [ shift ( x, y ) ] ]

        Layer nodeAttributes nodes ->
            group [] (List.map drawNode nodes)


main =
    let
        l1 =
            newLayer [ newRect 0 0 300 400 ]
    in
    canvas ( 600, 600 )
        []
        [ rect "blue" 100 100 []
        , drawNode l1
        ]



-- SVG CANVAS LIB


canvas ( w, h ) attrs =
    let
        ( x, y ) =
            ( -w / 2, -h / 2 )
    in
    svg
        (viewBox x y w h
            :: SA.shapeRendering "optimizeSpeed"
            :: HA.style "position" "fixed"
            :: HA.style "top" "0"
            :: HA.style "left" "0"
            :: HA.style "width" "100%"
            :: HA.style "height" "100%"
            :: attrs
        )


strokeWidth =
    fromFloat >> SA.strokeWidth


group =
    S.g


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


shift ( dx, dy ) t =
    { t | x = t.x + dx, y = t.y + dy }


transform =
    List.foldl (<|) identityTransform
        >> transformToString
        >> SA.transform


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
