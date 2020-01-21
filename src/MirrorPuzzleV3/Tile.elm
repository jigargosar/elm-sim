module MirrorPuzzleV3.Tile exposing (Path, Tile(..), getElementInLightSource, rotateElement, singletonPath, swapElements)

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


singletonPath : Int2 -> Direction -> Path
singletonPath int2 direction =
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
