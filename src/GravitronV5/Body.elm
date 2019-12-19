module GravitronV5.Body exposing (Body, toBody)

import GravitronV5.Id exposing (Id)
import GravitronV5.Tag exposing (Tag)
import Playground exposing (..)


type alias Body =
    { id : Id
    , tag : Tag
    , x : Number
    , y : Number
    , r : Number
    }


toBody : { a | id : Id, tag : Tag, x : Number, y : Number, r : Number } -> Body
toBody { id, tag, x, y, r } =
    Body id tag x y r
