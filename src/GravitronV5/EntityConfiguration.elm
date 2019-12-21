module GravitronV5.EntityConfiguration exposing (..)

import Playground exposing (..)


type PreStep name
    = ReceiveCollisionDamage (List name)
    | DieOnCollision (List name)
    | DieOnTimeout Int
    | RandomWalker


type Move name
    = GravitateTo name
    | BounceInScreen Int


type Step name
    = Move (Move name)
    | Fire name


type Death name
    = Spawn name


type alias EntityConfig name =
    { name : name
    , x : Number
    , y : Number
    , r : Number
    , vx : Number
    , vy : Number
    , color : Color
    , preStep : List (PreStep name)
    , step : List (Step name)
    , maxHP : Int
    , death : List (Death name)
    }


init : name -> (EntityConfig name -> EntityConfig name) -> EntityConfig name
init name func =
    let
        default : EntityConfig name
        default =
            { name = name
            , x = 0
            , y = 0
            , r = 10
            , vx = 0
            , vy = 0
            , color = blue
            , maxHP = 1
            , preStep = []
            , step = []
            , death = []
            }
    in
    func default
