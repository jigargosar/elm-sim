module MirrorPuzzleV3.Tile exposing (Tile(..), getLightPathUnfoldInstruction, getRefractionDirectionOfLightSource, rotateElement, swapElements)

-- Tile

import MirrorPuzzleV3.Graph as Graph
import Playground.Direction8 as Direction8 exposing (Direction8)


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


getRefractionDirectionOfLightSource : Tile -> Maybe (List Direction)
getRefractionDirectionOfLightSource =
    getElementInLightSource >> Maybe.map getRefractionDirectionsOfElement


getLightPathUnfoldInstruction : Tile -> Graph.UnfoldInstruction
getLightPathUnfoldInstruction tile =
    case tile of
        FilledContainer elementContainer element ->
            case elementContainer of
                LightSource ->
                    Graph.Stop

                Floor ->
                    Graph.Fork (getRefractionDirectionsOfElement element)

        EmptyContainer _ ->
            Graph.ContinuePrevious

        Goal ->
            Graph.Stop

        Wall ->
            Graph.Stop

        Hole ->
            Graph.ContinuePrevious


type ElementContainer
    = LightSource
    | Floor


type ElementType
    = Mirror
    | Prism



-- Direction


type alias Direction =
    Direction8


rotateDirectionBy : Int -> Direction -> Direction
rotateDirectionBy =
    Direction8.rotate


oppositeDirection : Direction -> Direction
oppositeDirection =
    Direction8.opposite



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
