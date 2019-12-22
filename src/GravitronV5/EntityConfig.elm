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

import GravitronV5.Names exposing (Name)
import Playground exposing (..)


type PreStep
    = ReceiveCollisionDamage (List Name)
    | DieOnCollision (List Name)
    | DieOnTimeout Int


type Move
    = GravitateTo Name
    | BounceInScreen Float
    | RandomWalker


type Step
    = Move Move
    | Fire Name Name


type Death
    = Spawn Name


type EntityConfig
    = EntityConfig Rec


type alias Rec =
    { name : Name
    , x : Number
    , y : Number
    , r : Number
    , vx : Number
    , vy : Number
    , color : Color
    , maxHP : Int
    , preSteps : List PreStep
    , steps : List Step
    , deathSteps : List Death
    }


named : Name -> (Rec -> Rec) -> EntityConfig
named name func =
    let
        default : Rec
        default =
            { name = name
            , x = 0
            , y = 0
            , r = 10
            , vx = 0
            , vy = 0
            , color = blue
            , maxHP = 1
            , preSteps = []
            , steps = []
            , deathSteps = []
            }
    in
    func default |> EntityConfig


map : (Rec -> Rec) -> EntityConfig -> EntityConfig
map func =
    unwrap >> func >> EntityConfig


unwrap : EntityConfig -> Rec
unwrap (EntityConfig rec) =
    rec


toRec : EntityConfig -> Rec
toRec =
    unwrap
