module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List.Extra
import Maybe.Extra
import Pivot exposing (Pivot)



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
    { add : Maybe String
    }


type alias PageItemsRecord =
    { groupId : GroupId }


type alias Model =
    { db : Db
    , page : Page
    }


initialModel : Model
initialModel =
    { db = emptyDb
    , page = PageGroups { add = Nothing }
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
    | AddGroupClicked


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
                            PageGroups { add = Nothing }

                        RouteItems groupId ->
                            PageItems { groupId = groupId }
            in
            pure { model | page = newPage }

        GotDB db ->
            pure { model | db = db }

        AddGroupClicked ->
            case model.page of
                PageGroups page ->
                    ( { model
                        | page =
                            let
                                newPage =
                                    { page | add = page.add |> Maybe.Extra.orElse (Just "") }
                            in
                            PageGroups newPage
                      }
                    , Cmd.none
                    )

                PageItems page ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


view : Model -> Html Msg
view model =
    case model.page of
        PageGroups _ ->
            viewGroupsPage model.db

        PageItems pr ->
            viewGroupItems model.db pr


viewGroupsPage : Db -> Html Msg
viewGroupsPage db =
    let
        viewGroupPivot : Pivot Group -> Html Msg
        viewGroupPivot groups =
            let
                viewGT : Group -> Html msg
                viewGT (Group { title }) =
                    div [ class "pointer pv1 ph2 br2" ] [ text title ]

                viewSGT : Group -> Html msg
                viewSGT (Group { title }) =
                    div
                        [ class "pointer pv1 ph2 br2"
                        , class "bg-blue white"
                        ]
                        [ text title ]
            in
            div []
                (Pivot.mapCS viewSGT viewGT groups
                    |> Pivot.toList
                )

        viewPT =
            div [ class "pv2 ttu tracked" ] [ text "Lists" ]

        viewEmptyGroups : Html Msg
        viewEmptyGroups =
            div [] [ text "empty" ]

        viewAddGroup : Html Msg
        viewAddGroup =
            button
                [ class "pointer bn ph2 pv1 f5 ttu bg-inherit blue"
                , onClick AddGroupClicked
                ]
                [ text "Add List" ]

        maybePivot =
            Pivot.fromList (allGroups db)
    in
    div [ class "measure-wide center" ]
        [ viewPT
        , case maybePivot of
            Just pivot ->
                viewGroupPivot pivot

            Nothing ->
                viewEmptyGroups
        , viewAddGroup
        ]


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
