module MirrorPuzzleV3.Main exposing (..)

-- TileGrid

import Dict exposing (Dict)
import Dict2d
import List2d exposing (List2d)
import MirrorPuzzleV3.Tile as Tile exposing (Tile(..))
import Number2 exposing (Int2)


type TileGrid
    = TileGrid Int2 (Dict Int2 Tile)


filledWith : Tile -> Int2 -> TileGrid
filledWith tile length2 =
    TileGrid length2 (Dict2d.filled tile length2)


fromList2d : List2d Tile -> TileGrid
fromList2d list2d =
    let
        ( length2, dict2d ) =
            Dict2d.fromList2dWithDefault Tile.Hole list2d
    in
    TileGrid length2 dict2d
