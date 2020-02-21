module LOL exposing (viewSample)

import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import List.Extra


type ItemId
    = ItemId Int


type GroupId
    = GroupId Int


type Group
    = Group GroupRecord


type alias GroupRecord =
    { id : GroupId
    , title : String
    }


type Item
    = Item ItemRecord


type alias ItemRecord =
    { id : ItemId
    , groupId : GroupId
    , title : String
    }


type alias Db =
    { groupDict : Dict Int Group
    , itemDict : Dict Int Item
    }


empty : Db
empty =
    { groupDict = Dict.empty
    , itemDict = Dict.empty
    }


toDb list =
    let
        insertGroupAndItems gidx ( groupTitle, itemTitles ) m =
            let
                gid =
                    GroupId gidx

                group : Group
                group =
                    Group { id = gid, title = groupTitle }

                insertItem idx title =
                    Dict.insert idx (Item { id = ItemId idx, groupId = gid, title = title })
            in
            { m
                | groupDict = Dict.insert gidx group m.groupDict
                , itemDict = List.Extra.indexedFoldl insertItem m.itemDict itemTitles
            }
    in
    List.Extra.indexedFoldl insertGroupAndItems empty list


findGroup : GroupId -> Db -> Maybe Group
findGroup (GroupId gidx) db =
    Dict.get gidx db.groupDict


itemGroupIdEq : GroupId -> Item -> Bool
itemGroupIdEq groupId (Item i) =
    groupId == i.groupId


findItemsInGroup : GroupId -> Db -> List Item
findItemsInGroup gid db =
    Dict.values db.itemDict
        |> List.filter (itemGroupIdEq gid)


viewSample : Html msg
viewSample =
    let
        ng =
            Tuple.pair

        ni =
            identity

        sampleData =
            [ ng "Inbox"
                [ ni "x: Show Group title with items"
                , ni "x: should we show group list separately? and on nav show children? Or show entire tree?"
                , ni "x: For now it seems easier to show everything"
                , ni "x: now showing focus is complicated, since we are showing both, group &  it's items"
                , ni "x: perhaps not, we can have group-pivot, and for that group, item pivot. humm.."
                , ni "x: I think we should keep things simple. one view for master list, then sub view for its list items"
                , ni "how do we model group list view, group items view?"
                , ni "perhaps we should use list & index? or have item store group id?"
                ]
            , ng "Projects"
                [ ni "LOL Demo"
                , ni "LOL: Focus Item/Group"
                ]
            ]

        db =
            toDb sampleData

        groupList =
            db.groupDict |> Dict.values

        gid =
            GroupId 0

        groupWithItems : Maybe ( Group, List Item )
        groupWithItems =
            findGroup gid db
                |> Maybe.map (\g -> ( g, findItemsInGroup gid db ))
    in
    div [ class "pv2 ph4" ]
        [ div [ class "pv2 f4 b" ] [ text "LOL Demo" ]
        , div [ class "pv2" ]
            [ div [ class "pv2 f4" ] [ text "Group Items List Page" ]
            , viewGroupItems groupWithItems
            ]
        , div [ class "pv2" ]
            [ div [ class "pv2 f4" ] [ text "Group Not Found Page" ]
            , viewGroupItems Nothing
            ]
        , div [ class "pv2" ]
            [ div [ class "pv2 f4" ] [ text "Group List Page" ]
            , viewGroupList groupList
            ]
        ]


viewGroupList =
    let
        viewGT (Group { title }) =
            div [] [ text title ]
    in
    List.map viewGT
        >> div []


viewGroupItems : Maybe ( Group, List Item ) -> Html msg
viewGroupItems mb =
    case mb of
        Nothing ->
            div [] [ text "INTERNAL ERROR" ]

        Just ( Group g, il ) ->
            let
                viewItem (Item i) =
                    div [ class "pv1 pl3" ] [ text i.title ]
            in
            div []
                (div [ class "b f5 pv2" ] [ text g.title ]
                    :: List.map viewItem il
                )
