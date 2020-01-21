module MirrorPuzzleV3.Tile exposing (Tile(..), rotateElement, swapElements)

-- Tile


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
