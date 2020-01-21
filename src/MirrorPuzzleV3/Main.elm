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
        lookupDict =
            List2d.toDict list2d

        length2 =
            ( List2d.maxWidth list2d, List2d.height list2d )
    in
    TileGrid length2 (Dict2d.resizeWithDefault Tile.Hole length2 lookupDict)
