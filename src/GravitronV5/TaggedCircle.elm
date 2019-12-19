module GravitronV5.TaggedCircle exposing (Record, TaggedCircle(..), toTaggedCircle)

import GravitronV5.Id exposing (Id)
import GravitronV5.Tag exposing (Tag)
import Playground exposing (..)


type TaggedCircle
    = TaggedCircle Record


type alias Record =
    { id : Id
    , tag : Tag
    , x : Number
    , y : Number
    , r : Number
    }


toTaggedCircle : { a | id : Id, tag : Tag, x : Number, y : Number, r : Number } -> TaggedCircle
toTaggedCircle { id, tag, x, y, r } =
    Record id tag x y r
        |> TaggedCircle
