module MirrorPuzzleV3.Tile exposing
    ( ElementType(..)
    , Tile(..)
    , decode
    , floor
    , getLightPathUnfoldInstruction
    , getRefractionDirectionOfLightSource
    , lightSourceWithMirror
    , mirror
    , prism
    , rotateElement
    , swapElements
    )

import MirrorPuzzleV3.Graph as Graph
import Playground.Direction8 as D exposing (Direction8)



-- Tile Constructors


floor : Tile
floor =
    EmptyContainer Floor


lightSourceWithMirror : Direction8 -> Tile
lightSourceWithMirror direction =
    FilledContainer LightSource (mirrorFacing direction)


prism : Direction8 -> Tile
prism direction =
    FilledContainer Floor (prismFasing direction)


mirror : Direction8 -> Tile
mirror direction =
    FilledContainer Floor (mirrorFacing direction)



-- Tile


type Tile
    = FilledContainer ElementContainer Element
    | EmptyContainer ElementContainer
    | Goal
    | Wall
    | Hole


decode : String -> Tile
decode encoded =
    let
        decodeDirection : Char -> Direction8
        decodeDirection =
            String.fromChar
                >> String.toInt
                >> Maybe.withDefault 0
                >> D.fromInt
    in
    case String.trim encoded |> String.toList of
        'S' :: [ char ] ->
            mirrorFacing (decodeDirection char)
                |> FilledContainer LightSource

        'S' :: _ ->
            EmptyContainer LightSource

        'P' :: [ char ] ->
            prismFasing (decodeDirection char)
                |> FilledContainer Floor

        'M' :: [ char ] ->
            mirrorFacing (decodeDirection char)
                |> FilledContainer Floor

        'D' :: _ ->
            Goal

        '|' :: _ ->
            Wall

        '_' :: _ ->
            EmptyContainer Floor

        _ ->
            Hole


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


getRefractionDirectionOfLightSource : Tile -> Maybe (List Direction8)
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


rotateDirectionBy : Int -> Direction8 -> Direction8
rotateDirectionBy =
    D.rotate


oppositeDirection : Direction8 -> Direction8
oppositeDirection =
    D.opposite



-- Element


type alias Element =
    { type_ : ElementType
    , direction : Direction8
    , movable : Bool
    }


mirrorFacing : Direction8 -> Element
mirrorFacing direction =
    Element Mirror direction True


prismFasing : Direction8 -> Element
prismFasing direction =
    Element Prism direction True


getRefractionDirectionsOfElement : Element -> List Direction8
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
