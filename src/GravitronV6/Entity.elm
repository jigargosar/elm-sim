module GravitronV6.Entity exposing (..)

import Playground exposing (..)


type PreStep
    = PreStep


type AliveStep
    = WalkRandomly


type DeathStep
    = DeathStep


type Phase
    = Spawning
    | Alive
    | Dying


type alias Entity =
    { id : Number
    , name : String
    , x : Number
    , y : Number
    , r : Number
    , color : Color
    , vx : Number
    , vy : Number
    , maxHP : Number
    , currentHP : Number
    , preSteps : List PreStep
    , aliveSteps : List AliveStep
    , deathSteps : List DeathStep
    , phase : Phase
    }


invalidId =
    -1


default : Entity
default =
    { id = invalidId
    , name = "Unknown"
    , x = 0
    , y = 0
    , r = 100
    , color = blue
    , vx = 0
    , vy = 0
    , maxHP = 1
    , currentHP = 1
    , preSteps = []
    , aliveSteps = []
    , deathSteps = []
    , phase = Alive
    }


performRandomWalk : Computer -> { c | x : Number, y : Number } -> { c | x : Number, y : Number }
performRandomWalk { time, screen } e =
    let
        ( x, y ) =
            ( wave screen.left screen.right 6 time
            , wave screen.top screen.bottom 8 time
            )
    in
    { e | x = x, y = y }
