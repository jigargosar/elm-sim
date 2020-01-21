module MirrorPuzzleV3.Tile exposing (Path, Tile(..), computeLightPath, getElementInLightSource, rotateElement, swapElements)

-- Tile

import Number2 exposing (Int2)


type Tile
    = FilledContainer ElementContainer Element
    | EmptyContainer ElementContainer
    | Goal
    | Wall
    | Hole


mapElement : (Element -> Element) -> Tile -> Maybe Tile
mapElement func tile =
    case tile of
        FilledContainer container element ->
            Just (FilledContainer container (func element))

        _ ->
            Nothing


rotateElement : Tile -> Maybe Tile
rotateElement =
    mapElement rotateElement_


swapElements : Tile -> Tile -> Maybe ( Tile, Tile )
swapElements dragTile dropTile =
    case ( dragTile, dropTile ) of
        ( FilledContainer container1 element, EmptyContainer container2 ) ->
            Just ( EmptyContainer container1, FilledContainer container2 element )

        _ ->
            Nothing


getElementInLightSource : Tile -> Maybe Element
getElementInLightSource tile =
    case tile of
        FilledContainer LightSource element ->
            Just element

        _ ->
            Nothing


getRefractionDirections : Tile -> List Direction
getRefractionDirections =
    getElement >> Maybe.map getRefractionDirectionsOfElement >> Maybe.withDefault []


getElement : Tile -> Maybe Element
getElement tile =
    case tile of
        FilledContainer _ element ->
            Just element

        _ ->
            Nothing


type ElementContainer
    = LightSource
    | Floor


type ElementType
    = Mirror
    | Prism



-- Direction


type Direction
    = Direction Int


rotateDirectionBy : Int -> Direction -> Direction
rotateDirectionBy steps (Direction zeroToSevenInt) =
    Direction (modBy 8 (zeroToSevenInt + steps))


oppositeDirection : Direction -> Direction
oppositeDirection =
    rotateDirectionBy 4



-- Path


type PathElement
    = PathElement Int2 Direction
    | PathEnd Int2


type Path
    = Path (List PathElement)
    | Fork (List Path)


computeLightPath : (Int2 -> Maybe Tile) -> Int2 -> Maybe Path
computeLightPath tileAt startPosition =
    let
        foo element =
            getRefractionDirectionsOfElement element
                |> List.map
                    (\dir ->
                        let
                            pe =
                                PathElement startPosition dir
                        in
                        computeLightPathHelp tileAt pe (Path [ pe ])
                    )
                |> Fork
    in
    tileAt startPosition
        |> Maybe.andThen getElementInLightSource
        |> Maybe.map foo


computeLightPathHelp : (Int2 -> Maybe Tile) -> PathElement -> Path -> Path
computeLightPathHelp tileAt lastPE =
    case lastPE of
        PathEnd _ ->
            identity

        PathElement index direction ->
            let
                nextIndex =
                    stepIndexInDirection direction index
            in
            case tileAt nextIndex of
                Nothing ->
                    identity

                Just tile ->
                    case tile of
                        FilledContainer elementContainer element ->
                            pathCons lastPE >> computeLightPathHelp tileAt lastPE

                        EmptyContainer elementContainer ->
                            pathCons lastPE >> computeLightPathHelp tileAt lastPE

                        Goal ->
                            pathCons lastPE >> computeLightPathHelp tileAt lastPE

                        Wall ->
                            pathCons lastPE >> computeLightPathHelp tileAt lastPE

                        Hole ->
                            pathCons lastPE >> computeLightPathHelp tileAt lastPE


stepIndexInDirection : Direction -> Int2 -> Int2
stepIndexInDirection =
    Debug.todo "impl"


pathCons : PathElement -> Path -> Path
pathCons e path =
    case path of
        Path single ->
            Path (e :: single)

        Fork multiple ->
            Fork (List.map (pathCons e) multiple)


pathCons2 : PathElement -> PathElement -> Path -> Path
pathCons2 e1 e2 path =
    Fork [ pathCons e1 path, pathCons e2 path ]


pathMember : PathElement -> Path -> Bool
pathMember e path =
    case path of
        Path single ->
            List.member e single

        Fork multiple ->
            List.any (pathMember e) multiple



-- Element


type alias Element =
    { type_ : ElementType
    , direction : Direction
    , movable : Bool
    }


getRefractionDirectionsOfElement : Element -> List Direction
getRefractionDirectionsOfElement element =
    case element.type_ of
        Mirror ->
            [ element.direction ]

        Prism ->
            [ element.direction, oppositeDirection element.direction ]


rotateElementBy : Int -> Element -> Element
rotateElementBy steps element =
    { element | direction = rotateDirectionBy steps element.direction }


rotateElement_ : Element -> Element
rotateElement_ =
    rotateElementBy 1
