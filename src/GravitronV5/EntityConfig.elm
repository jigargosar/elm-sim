module GravitronV5.EntityConfig exposing
    ( Death(..)
    , EntityConfig
    , Move(..)
    , PreStep(..)
    , Rec
    , Step(..)
    , map
    , named
    , toRec
    )

import Playground exposing (..)


type PreStep name
    = ReceiveCollisionDamage (List name)
    | DieOnCollision (List name)
    | DieOnTimeout Int


type Move name
    = GravitateTo name
    | BounceInScreen Int
    | RandomWalker


type Step name
    = Move (Move name)
    | Fire name


type Death name
    = Spawn name


type EntityConfig name
    = EntityConfig (Rec name)


type alias Rec name =
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


named : name -> (Rec name -> Rec name) -> EntityConfig name
named name func =
    let
        default : Rec name
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
    func default |> EntityConfig


map : (Rec name -> Rec name) -> EntityConfig name -> EntityConfig name
map func =
    unwrap >> func >> EntityConfig


unwrap : EntityConfig name -> Rec name
unwrap (EntityConfig rec) =
    rec


toRec : EntityConfig name -> Rec name
toRec =
    unwrap
