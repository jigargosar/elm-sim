module Main exposing (main)

import MainSvgCanvas as MC
import String


type alias NodeAttributes =
    { id : String
    , x : Float
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
    Layer (NodeAttributes "" 0 0 0 0 "")


newRect w h =
    NodeAttributes "" 0 0 w h "red" |> Node


drawNode node =
    case node of
        Node shape ->
            let
                { x, y, width, height } =
                    shape
            in
            MC.rect shape.fill width height [ MC.transform [ MC.shift ( x, y ) ] ]

        Layer nodeAttributes nodes ->
            MC.group [] (List.map drawNode nodes)


main =
    let
        l1 =
            newLayer [ newRect 300 400 ]
    in
    MC.canvas ( 600, 600 )
        []
        [ MC.rect "blue" 100 100 []
        , drawNode l1
        ]
