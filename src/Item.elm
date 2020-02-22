module Item exposing (Item, id, idEq, random, title)

import GroupId exposing (GroupId)
import ItemId exposing (ItemId)
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


title : Item -> String
title (Item i) =
    i.title


random : GroupId -> String -> Generator Item
random gid iTitle =
    ItemId.random
        |> Random.map (\iid -> Item { id = iid, groupId = gid, title = iTitle })
