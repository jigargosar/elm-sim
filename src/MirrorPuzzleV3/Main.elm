module MirrorPuzzleV3.Main exposing (..)


type Tile
    = FilledContainer ElementContainer Element
    | EmptyContainer ElementContainer
    | Goal
    | Wall
    | Hole


type ElementContainer
    = LightSource
    | Floor


type ElementType
    = Mirror
    | Prism


type Direction
    = Direction Int


type alias Element =
    { type_ : ElementType
    , direction : Direction
    , movable : Bool
    }


rotateElementBy : Int -> Element -> Element
rotateElementBy steps element =
    Debug.todo "impl"


mapElement : (Element -> Element) -> Tile -> Maybe Tile
mapElement func tile =
    case tile of
        FilledContainer container element ->
            Just (FilledContainer container (func element))

        _ ->
            Nothing


swapElementInTiles : Tile -> Tile -> Maybe ( Tile, Tile )
swapElementInTiles dragTile dropTile =
    case ( dragTile, dropTile ) of
        ( FilledContainer container1 element, EmptyContainer container2 ) ->
            Just ( EmptyContainer container1, FilledContainer container2 element )

        _ ->
            Nothing
