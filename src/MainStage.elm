module MainStage exposing (..)

import MainSvgCanvas as MC


type alias NodeAttributes =
    { id : String
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    , fill : String
    }


type Node
    = Group NodeAttributes (List Node)
    | Node NodeAttributes


group : List Node -> Node
group =
    Group (NodeAttributes "" 0 0 0 0 "")


rect w h =
    NodeAttributes "" 0 0 w h "red" |> Node


drawNode node =
    case node of
        Node shape ->
            let
                { x, y, width, height } =
                    shape
            in
            MC.rect shape.fill width height [ MC.transform [ MC.shift ( x, y ) ] ]

        Group nodeAttributes nodes ->
            MC.group [] (List.map drawNode nodes)


stage ( w, h ) nodes =
    MC.canvas ( w, h )
        []
        (List.map drawNode nodes)
