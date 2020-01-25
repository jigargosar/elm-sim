module MirrorPuzzleV3.TileGird exposing
    ( TileGrid
    , computeLightPaths
    , decode
    , dimensions
    , filledWith
    , fromList2d
    , rotateElement
    , swapElements
    , toList
    )

-- TileGrid

import Dict exposing (Dict)
import Dict2d
import List2d exposing (List2d)
import MirrorPuzzleV3.Graph as Graph
import MirrorPuzzleV3.Tile as Tile exposing (Tile(..))
import Number2 exposing (Int2)


type TileGrid
    = TileGrid Int2 (Dict Int2 Tile)


dimensions : TileGrid -> Int2
dimensions (TileGrid length2 _) =
    length2


filledWith : Tile -> Int2 -> TileGrid
filledWith tile dim =
    TileGrid dim (Dict2d.filled tile dim)


fromList2d : List2d Tile -> TileGrid
fromList2d list2d =
    let
        ( dim, dict2d ) =
            Dict2d.fromListsWithDefault Tile.Hole list2d
    in
    TileGrid dim dict2d


decode : String -> TileGrid
decode encoded =
    let
        ( dim, dict2d ) =
            Dict2d.decodeCSV Tile.decode encoded
    in
    TileGrid dim dict2d


rotateElement : Int2 -> TileGrid -> Maybe TileGrid
rotateElement index2d =
    maybeMapDict2d
        (Dict2d.maybeMapAt index2d Tile.rotateElement)


swapElements : Int2 -> Int2 -> TileGrid -> Maybe TileGrid
swapElements idxA idxB =
    maybeMapDict2d
        (Dict2d.maybeMapAt2 idxA idxB Tile.swapElements)


maybeMapDict2d : (Dict Int2 Tile -> Maybe (Dict Int2 Tile)) -> TileGrid -> Maybe TileGrid
maybeMapDict2d func (TileGrid dim dict2d) =
    Maybe.map (TileGrid dim) (func dict2d)


computeLightPaths : TileGrid -> List Graph.Graph
computeLightPaths (TileGrid _ dict) =
    let
        unfoldInstructionAt : Int2 -> Graph.UnfoldInstruction
        unfoldInstructionAt position =
            case Dict.get position dict of
                Just tile ->
                    Tile.getLightPathUnfoldInstruction tile

                Nothing ->
                    Graph.Stop

        graphStartingAt ( position, el ) =
            Tile.getRefractionDirectionOfLightSource el
                |> Maybe.map (Graph.unfold unfoldInstructionAt position)
    in
    Dict.toList dict |> List.filterMap graphStartingAt


toList : TileGrid -> List ( Int2, Tile )
toList (TileGrid _ dict) =
    Dict.toList dict
