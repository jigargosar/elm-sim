module MirrorPuzzleV3.Tile exposing
    ( Element
    , ElementContainer(..)
    , ElementType(..)
    , Tile(..)
    , decode
    , floor
    , getLightPathNodeType
    , getRefractionDirectionOfLightSource
    , isMovable
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
    EmptyContainer Platform


lightSourceWithMirror : Direction8 -> Tile
lightSourceWithMirror direction =
    FilledContainer LightSource (mirrorFacing direction)


prism : Direction8 -> Tile
prism direction =
    FilledContainer Platform (prismFasing direction)


mirror : Direction8 -> Tile
mirror direction =
    FilledContainer Platform (mirrorFacing direction)



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
                |> FilledContainer Platform

        'M' :: [ char ] ->
            mirrorFacing (decodeDirection char)
                |> FilledContainer Platform

        'D' :: _ ->
            Goal

        '|' :: _ ->
            Wall

        '_' :: _ ->
            EmptyContainer Platform

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


isMovable : Tile -> Bool
isMovable tile =
    case tile of
        FilledContainer _ element ->
            element.movable

        _ ->
            False


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


getLightPathNodeType : Tile -> Maybe Graph.NodeType
getLightPathNodeType tile =
    case tile of
        FilledContainer elementContainer element ->
            case elementContainer of
                LightSource ->
                    Nothing

                Platform ->
                    Just (Graph.BranchNode (getRefractionDirectionsOfElement element))

        EmptyContainer _ ->
            Just Graph.ContinuePreviousDirectionNode

        Goal ->
            Just Graph.LeafNode

        Wall ->
            Nothing

        Hole ->
            Just Graph.ContinuePreviousDirectionNode


type ElementContainer
    = LightSource
    | Platform


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
