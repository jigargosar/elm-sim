module GravitronV5.Player exposing (Player, init)

import GravitronV5.Id as Id exposing (Id)
import GravitronV5.Tag as Tag exposing (Tag)
import Playground exposing (..)


type alias Player =
    { id : Id
    , tag : Tag
    , x : Number
    , y : Number
    , r : Number
    , vx : Number
    , vy : Number
    }


init : Number -> Number -> Player
init x y =
    let
        initialPlayerRadius =
            20
    in
    Player Id.Player Tag.TagPlayer x y initialPlayerRadius 0 0
