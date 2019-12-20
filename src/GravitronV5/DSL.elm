module GravitronV5.DSL exposing (..)

import Playground exposing (..)
import Random


type Name
    = Player
    | Turret
    | Bullet


type UUID
    = UUID Int


randomUUID : Random.Generator UUID
randomUUID =
    Random.int 100 Random.maxInt
        |> Random.map UUID


type alias CommonProps =
    { uuid : UUID
    , x : Number
    , y : Number
    , r : Number
    , vx : Number
    , vy : Number
    , color : Color
    }


commonPropGenerator : Random.Generator CommonProps
commonPropGenerator =
    let
        initCommonProps : UUID -> CommonProps
        initCommonProps uuid =
            { uuid = uuid, x = 0, y = 0, r = 0, vx = 0, vy = 0, color = blue }
    in
    randomUUID |> Random.map initCommonProps
