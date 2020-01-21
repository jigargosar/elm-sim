module MirrorPuzzleV3.Main exposing (..)

-- TileGrid

import Dict exposing (Dict)
import Int2
import MirrorPuzzleV3.Tile exposing (Tile(..))
import Number2 as NT exposing (Int2)


type TileGrid
    = TileGrid Int2 (Dict Int2 Tile)


filledWith : Tile -> Int2 -> TileGrid
filledWith tile dimensions =
    TileGrid dimensions (Int2.toDict (always tile) dimensions)
