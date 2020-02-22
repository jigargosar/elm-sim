module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import List.Extra



-- DB


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


type Db
    = Db DbRecord


type alias DbRecord =
    { groupDict : Dict Int Group
    , itemDict : Dict Int Item
    }


emptyDb : Db
emptyDb =
    Db
        { groupDict = Dict.empty
        , itemDict = Dict.empty
        }


dbFromList : List ( String, List String ) -> Db
dbFromList list =
    let
        insertGroupAndItems : Int -> ( String, List String ) -> Db -> Db
        insertGroupAndItems gidx ( groupTitle, itemTitles ) (Db m) =
            let
                gid =
                    GroupId gidx

                group : Group
                group =
                    Group { id = gid, title = groupTitle }

                insertItem idx title =
                    Dict.insert idx (Item { id = ItemId idx, groupId = gid, title = title })
            in
            Db
                { m
                    | groupDict = Dict.insert gidx group m.groupDict
                    , itemDict = List.Extra.indexedFoldl insertItem m.itemDict itemTitles
                }
    in
    List.Extra.indexedFoldl insertGroupAndItems emptyDb list


findGroup : GroupId -> Db -> Maybe Group
findGroup (GroupId gidx) (Db db) =
    Dict.get gidx db.groupDict


allGroups : Db -> List Group
allGroups (Db db) =
    db.groupDict |> Dict.values


itemGroupIdEq : GroupId -> Item -> Bool
itemGroupIdEq groupId (Item i) =
    groupId == i.groupId


findItemsInGroup : GroupId -> Db -> List Item
findItemsInGroup gid (Db db) =
    Dict.values db.itemDict
        |> List.filter (itemGroupIdEq gid)



-- Model


type Route
    = RouteGroups
    | RouteItems GroupId


type Page
    = PageGroups PageGroupsRecord
    | PageItems PageItemsRecord


type alias PageGroupsRecord =
    {}


type alias PageItemsRecord =
    { groupId : GroupId }


type alias Model =
    { db : Db
    , page : Page
    }


initialModel : Model
initialModel =
    { db = emptyDb
    , page = PageGroups {}
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.none
    )



-- Update


type Msg
    = RouteTo Route
    | GotDB Db


pure : a -> ( a, Cmd msg )
pure m =
    ( m, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RouteTo route ->
            let
                newPage =
                    case route of
                        RouteGroups ->
                            PageGroups {}

                        RouteItems groupId ->
                            PageItems { groupId = groupId }
            in
            pure { model | page = newPage }

        GotDB db ->
            pure { model | db = db }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


view : Model -> Html Msg
view model =
    case model.page of
        PageGroups _ ->
            viewGroupList (allGroups model.db)

        PageItems pr ->
            viewGroupItems model.db pr


viewGroupList : List Group -> Html Msg
viewGroupList =
    let
        viewGT (Group { title }) =
            div [ class "pointer b pa2 underline hover-bg-blue hover-white br2" ] [ text title ]
    in
    List.map viewGT
        >> div []


viewGroupItems : Db -> PageItemsRecord -> Html Msg
viewGroupItems db { groupId } =
    case
        findGroup groupId db
            |> Maybe.map (\g -> ( g, findItemsInGroup groupId db ))
    of
        Nothing ->
            div [ class "f2 red" ] [ text ("Group Not Found: " ++ Debug.toString groupId) ]

        Just ( Group g, il ) ->
            let
                viewItem (Item i) =
                    div [ class "pv1 pl3" ] [ text i.title ]
            in
            div []
                (div [ class "b f5 pv2" ] [ text g.title ]
                    :: List.map viewItem il
                )



-- Main


sampleDb =
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
                [ ni "Db Demo"
                , ni "Db: Focus Item/Group"
                ]
            ]
    in
    dbFromList sampleData



--noinspection ElmUnusedSymbol


viewSample : Html Msg
viewSample =
    let
        sampleModel =
            { initialModel | db = sampleDb }
    in
    div [ class "pv2 ph4" ]
        [ div [ class "pv2 f4 b" ] [ text "LOL Demo" ]
        , div [ class "pv2" ]
            [ div [ class "pv2 f4 ttu " ] [ text "Items Page" ]
            , view { sampleModel | page = PageItems { groupId = GroupId 0 } }
            ]
        , div [ class "pv2" ]
            [ div [ class "pv2 f4 ttu " ] [ text "Items Page: Group Not Found" ]
            , view { sampleModel | page = PageItems { groupId = GroupId -1 } }
            ]
        , div [ class "pv2" ]
            [ div [ class "pv2 f4 ttu" ] [ text "Groups Page" ]
            , view { sampleModel | page = PageGroups {} }
            ]
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { -- init = init
          init = init >> Tuple.mapFirst (\m -> { m | db = sampleDb })
        , view = view

        --, view = always viewSample
        , update = update
        , subscriptions = subscriptions
        }
