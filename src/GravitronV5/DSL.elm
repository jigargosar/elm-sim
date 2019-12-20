module GravitronV5.DSL exposing (..)

import Playground exposing (..)


type Name
    = Player
    | Turret
    | Bullet


type alias CommonProps =
    { uuid : Int
    , x : Number
    , y : Number
    , r : Number
    , vx : Number
    , vy : Number
    , color : Color
    }
