module Item exposing (Item, groupId, groupIdEq, id, idEq, idString, random, title)

import GroupId as GI exposing (GroupId)
import ItemId as II exposing (ItemId)
import Random exposing (Generator)


type Item
    = Item ItemRecord


type alias ItemRecord =
    { id : ItemId
    , groupId : GroupId
    , title : String
    }


id : Item -> ItemId
id (Item i) =
    i.id


idEq : ItemId -> Item -> Bool
idEq itemId (Item i) =
    itemId == i.id


groupIdEq : GroupId -> Item -> Bool
groupIdEq gid =
    groupId >> eq gid


idString : Item -> String
idString =
    id >> II.toString


eq =
    (==)


groupId : Item -> GroupId
groupId (Item i) =
    i.groupId


title : Item -> String
title (Item i) =
    i.title


random : GroupId -> String -> Generator Item
random gid iTitle =
    II.random
        |> Random.map (\iid -> Item { id = iid, groupId = gid, title = iTitle })
