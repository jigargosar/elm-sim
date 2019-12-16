module GravitronV3.Tag exposing (Tag(..), TaggedCircle, circular)

import GravitronV3.Point exposing (Point)
import GravitronV3.RigidBody exposing (Circular)


type Tag
    = Bullet
    | Turret
    | Player


type alias TaggedCircle =
    { tag : Tag
    , position : Point
    , radius : Float
    }


circular : Tag -> Circular a -> TaggedCircle
circular tag { position, radius } =
    TaggedCircle tag position radius
