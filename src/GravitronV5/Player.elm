module GravitronV5.Player exposing (Player)

import GravitronV5.Id exposing (Id)
import GravitronV5.Tag exposing (Tag)
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
