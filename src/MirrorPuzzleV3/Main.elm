module MirrorPuzzleV3.Main exposing (..)

-- TileGrid

import Dict exposing (Dict)
import MirrorPuzzleV3.Tile exposing (Tile(..))
import Number2 as NT exposing (Int2)


type TileGrid
    = TileGrid Int2 (Dict Int2 Tile)


filledWith : Tile -> Int2 -> TileGrid
filledWith tile dimensions =
    let
        insertHole pos =
            Dict.insert pos tile
    in
    TileGrid dimensions (NT.foldIndices2d insertHole Dict.empty dimensions)
