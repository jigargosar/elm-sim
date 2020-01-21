module MirrorPuzzleV3.Tile exposing (..)

-- Tile


type Tile
    = FilledContainer ElementContainer Element
    | EmptyContainer ElementContainer
    | Goal
    | Wall
    | Hole


mapElementInTile : (Element -> Element) -> Tile -> Maybe Tile
mapElementInTile func tile =
    case tile of
        FilledContainer container element ->
            Just (FilledContainer container (func element))

        _ ->
            Nothing


rotateElementInTile : Tile -> Maybe Tile
rotateElementInTile =
    mapElementInTile rotateElement


swapElementInTiles : Tile -> Tile -> Maybe ( Tile, Tile )
swapElementInTiles dragTile dropTile =
    case ( dragTile, dropTile ) of
        ( FilledContainer container1 element, EmptyContainer container2 ) ->
            Just ( EmptyContainer container1, FilledContainer container2 element )

        _ ->
            Nothing


getElementContainedInLightSource : Tile -> Maybe Element
getElementContainedInLightSource tile =
    case tile of
        FilledContainer LightSource element ->
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



-- Element


type alias Element =
    { type_ : ElementType
    , direction : Direction
    , movable : Bool
    }


rotateElementBy : Int -> Element -> Element
rotateElementBy steps element =
    { element | direction = rotateDirectionBy steps element.direction }


rotateElement : Element -> Element
rotateElement =
    rotateElementBy 1
