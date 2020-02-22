module GroupId exposing (GroupId, random, toString)

import Random exposing (Generator)
import String exposing (fromInt)


type GroupId
    = GroupId Int


random : Generator GroupId
random =
    Random.int 0 10000000 |> Random.map GroupId


toString : GroupId -> String.String
toString (GroupId i) =
    "gid_" ++ fromInt i
