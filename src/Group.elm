module Group exposing (Group, id, idEq, idString, random, title)

import GroupId as GI exposing (GroupId)
import Random exposing (Generator)


type Group
    = Group GroupRecord


type alias GroupRecord =
    { id : GroupId
    , title : String
    }


id : Group -> GroupId
id (Group g) =
    g.id


idString : Group -> String
idString =
    id >> GI.toString


idEq : GroupId -> Group -> Bool
idEq groupId (Group g) =
    groupId == g.id


title : Group -> String
title (Group g) =
    g.title


random : String -> Generator Group
random gTitle =
    GI.random
        |> Random.map (\gid -> Group { id = gid, title = gTitle })
