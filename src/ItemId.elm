module ItemId exposing (ItemId, random, toString)

import Random exposing (Generator)
import String exposing (fromInt)


type ItemId
    = ItemId Int


random : Generator ItemId
random =
    Random.int 0 10000000 |> Random.map ItemId


toString : ItemId -> String.String
toString (ItemId i) =
    "gid_" ++ fromInt i
