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


setSize ( w, h ) node =
    case node of
        Group na l ->
            Group { na | width = w, height = h } l

        Node na ->
            Node { na | width = w, height = h }


getSize node =
    case node of
        Group na _ ->
            ( na.width, na.height )

        Node na ->
            ( na.width, na.height )


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


render node =
    MC.canvas (getSize node) [] [ drawNode node ]
