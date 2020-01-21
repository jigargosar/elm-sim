module MirrorPuzzleV3.Main exposing (filledWith, fromList2d)

-- TileGrid

import Dict exposing (Dict)
import Dict2d
import List2d exposing (List2d)
import MirrorPuzzleV3.Tile as Tile exposing (Tile(..))
import Number2 exposing (Int2)


type TileGrid
    = TileGrid Int2 (Dict Int2 Tile)


filledWith : Tile -> Int2 -> TileGrid
filledWith tile dim =
    TileGrid dim (Dict2d.filled tile dim)


fromList2d : List2d Tile -> TileGrid
fromList2d list2d =
    let
        ( dim, dict2d ) =
            Dict2d.fromList2dWithDefault Tile.Hole list2d
    in
    TileGrid dim dict2d


rotateElement : Int2 -> TileGrid -> Maybe TileGrid
rotateElement index2d =
    maybeMapDict2d
        (Dict2d.maybeMapAt index2d
            Tile.rotateElementInTile
        )


swapElements : Int2 -> Int2 -> TileGrid -> Maybe TileGrid
swapElements idxA idxB =
    maybeMapDict2d (Dict2d.maybeMapAt2 idxA idxB Tile.swapElementInTiles)


maybeMapDict2d : (Dict Int2 Tile -> Maybe (Dict Int2 Tile)) -> TileGrid -> Maybe TileGrid
maybeMapDict2d func (TileGrid dim dict2d) =
    func dict2d
        |> Maybe.map (TileGrid dim)
