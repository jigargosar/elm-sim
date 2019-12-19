module GravitronV5.TaggedCircle exposing (TaggedCircle, toTaggedCircle)

import GravitronV5.Id exposing (Id)
import GravitronV5.Tag exposing (Tag)
import Playground exposing (..)


type alias TaggedCircle =
    { id : Id
    , tag : Tag
    , x : Number
    , y : Number
    , r : Number
    }


toTaggedCircle : { a | id : Id, tag : Tag, x : Number, y : Number, r : Number } -> TaggedCircle
toTaggedCircle { id, tag, x, y, r } =
    TaggedCircle id tag x y r
