module Group exposing (Group, id, idEq, random, title)

import GroupId exposing (GroupId)
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


idEq : GroupId -> Group -> Bool
idEq groupId (Group g) =
    groupId == g.id


title : Group -> String
title (Group g) =
    g.title


random : String -> Generator Group
random gTitle =
    GroupId.random
        |> Random.map (\gid -> Group { id = gid, title = gTitle })
