module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (autofocus, class, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import List.Extra
import Maybe.Extra
import Pivot exposing (Pivot)
import String.Extra



-- DB


type ItemId
    = ItemId Int


type GroupId
    = GroupId Int


type Group
    = Group GroupRecord


groupGroupId (Group g) =
    g.id


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


groupIdEq : GroupId -> Group -> Bool
groupIdEq groupId (Group g) =
    groupId == g.id


findItemsInGroup : GroupId -> Db -> List Item
findItemsInGroup gid (Db db) =
    Dict.values db.itemDict
        |> List.filter (itemGroupIdEq gid)


dbAddNewGroup : String -> Db -> Db
dbAddNewGroup groupTitle (Db db) =
    let
        gidx =
            allGroups (Db db)
                |> List.map
                    (\(Group { id }) ->
                        let
                            (GroupId idx) =
                                id
                        in
                        idx
                    )
                |> List.maximum
                |> Maybe.withDefault 0
                |> (+) 1

        gid =
            GroupId gidx

        group : Group
        group =
            Group { id = gid, title = groupTitle }
    in
    Db { db | groupDict = Dict.insert gidx group db.groupDict }



-- Model


type Route
    = RouteGroups
    | RouteItems GroupId


type Page
    = PageGroups PageGroupsRecord
    | PageItems PageItemsRecord


type alias Add =
    { content : String
    }


type alias PageGroupsRecord =
    { add : Maybe Add
    , selectedGroupId : Maybe GroupId
    }


type alias PageItemsRecord =
    { groupId : GroupId, selectedItemId : Maybe ItemId }


type alias Model =
    { db : Db
    , page : Page
    }


initialModel : Model
initialModel =
    { db = emptyDb
    , page = PageGroups { add = Nothing, selectedGroupId = Nothing }
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
    | InputChanged String
    | SubmitClicked
    | CancelClicked
    | OnKeyDown String String


pure : a -> ( a, Cmd msg )
pure m =
    ( m, Cmd.none )


switchRoute : Route -> Model -> ( Model, Cmd Msg )
switchRoute route model =
    let
        newPage =
            case route of
                RouteGroups ->
                    PageGroups { add = Nothing, selectedGroupId = Nothing }

                RouteItems groupId ->
                    PageItems { groupId = groupId, selectedItemId = Nothing }
    in
    pure { model | page = newPage }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RouteTo route ->
            switchRoute route model

        GotDB db ->
            pure { model | db = db }

        AddGroupClicked ->
            case model.page of
                PageGroups page ->
                    ( { model
                        | page =
                            let
                                newPage =
                                    { page
                                        | add =
                                            page.add
                                                |> Maybe.Extra.orElse (Just { content = "" })
                                    }
                            in
                            PageGroups newPage
                      }
                    , Cmd.none
                    )

                PageItems _ ->
                    ( model, Cmd.none )

        InputChanged string ->
            case model.page of
                PageGroups page ->
                    case page.add of
                        Nothing ->
                            ( model, Cmd.none )

                        Just add ->
                            ( { model
                                | page =
                                    let
                                        newPage =
                                            { page | add = Just { add | content = string } }
                                    in
                                    PageGroups newPage
                              }
                            , Cmd.none
                            )

                PageItems _ ->
                    ( model, Cmd.none )

        SubmitClicked ->
            case model.page of
                PageGroups page ->
                    ( { model
                        | page =
                            let
                                newPage =
                                    { page | add = Nothing }
                            in
                            PageGroups newPage
                        , db =
                            let
                                newDb =
                                    case page.add |> Maybe.andThen (.content >> String.trim >> String.Extra.nonBlank) of
                                        Just string ->
                                            dbAddNewGroup string model.db

                                        Nothing ->
                                            model.db
                            in
                            newDb
                      }
                    , Cmd.none
                    )

                PageItems _ ->
                    ( model, Cmd.none )

        CancelClicked ->
            case model.page of
                PageGroups page ->
                    ( { model
                        | page =
                            let
                                newPage =
                                    { page | add = Nothing }
                            in
                            PageGroups newPage
                      }
                    , Cmd.none
                    )

                PageItems _ ->
                    ( model, Cmd.none )

        OnKeyDown keyName tagName ->
            case model.page of
                PageGroups page ->
                    case ( keyName, tagName ) of
                        ( _, "INPUT" ) ->
                            ( model, Cmd.none )

                        ( "j", _ ) ->
                            ( { model
                                | page =
                                    let
                                        selectedGroupId =
                                            Pivot.fromList (allGroups model.db)
                                                |> Maybe.map
                                                    (\p ->
                                                        case page.selectedGroupId of
                                                            Just gid ->
                                                                Pivot.withRollback (Pivot.firstWith (groupIdEq gid)) p

                                                            Nothing ->
                                                                p
                                                    )
                                                |> Maybe.map (Pivot.withRollback Pivot.goR)
                                                |> Maybe.map (Pivot.getC >> groupGroupId)

                                        newPage =
                                            { page | selectedGroupId = selectedGroupId }
                                    in
                                    PageGroups newPage
                              }
                            , Cmd.none
                            )

                        ( "k", _ ) ->
                            ( { model
                                | page =
                                    let
                                        selectedGroupId =
                                            Pivot.fromList (allGroups model.db)
                                                |> Maybe.map
                                                    (\p ->
                                                        case page.selectedGroupId of
                                                            Just gid ->
                                                                Pivot.withRollback (Pivot.firstWith (groupIdEq gid)) p

                                                            Nothing ->
                                                                p
                                                    )
                                                |> Maybe.map (Pivot.withRollback Pivot.goL)
                                                |> Maybe.map (Pivot.getC >> groupGroupId)

                                        newPage =
                                            { page | selectedGroupId = selectedGroupId }
                                    in
                                    PageGroups newPage
                              }
                            , Cmd.none
                            )

                        ( "l", _ ) ->
                            let
                                maybeRoute =
                                    Pivot.fromList (allGroups model.db)
                                        |> Maybe.map
                                            (\p ->
                                                case page.selectedGroupId of
                                                    Just gid ->
                                                        Pivot.withRollback (Pivot.firstWith (groupIdEq gid)) p

                                                    Nothing ->
                                                        p
                                            )
                                        |> Maybe.map (Pivot.getC >> groupGroupId >> RouteItems)
                            in
                            case maybeRoute of
                                Nothing ->
                                    ( model, Cmd.none )

                                Just route ->
                                    switchRoute route model

                        _ ->
                            ( model, Cmd.none )

                PageItems _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown
            (JD.map2 OnKeyDown
                (JD.field "key" JD.string)
                (JD.at [ "target", "tagName" ] JD.string)
            )
        ]



-- View


view : Model -> Html Msg
view model =
    case model.page of
        PageGroups page ->
            viewGroupsPage model.db page

        PageItems pr ->
            viewGroupItems model.db pr


viewGroupsPage : Db -> PageGroupsRecord -> Html Msg
viewGroupsPage db page =
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

        btnStyle1 =
            class "pointer bn ph2 pv1 f5 ttu bg-inherit blue"

        viewAddGroupButton : Html Msg
        viewAddGroupButton =
            button
                [ btnStyle1
                , onClick AddGroupClicked
                ]
                [ text "Add List" ]

        viewAddGroupInlineForm : Add -> Html Msg
        viewAddGroupInlineForm add =
            div [ class "" ]
                [ input
                    [ class "w-100 ph2 pv0 lh-solid"
                    , value add.content
                    , onInput InputChanged
                    , onEnter SubmitClicked
                    , autofocus True
                    ]
                    []
                , div []
                    [ button [ btnStyle1, onClick SubmitClicked ] [ text "Add" ]
                    , button [ btnStyle1, onClick CancelClicked ] [ text "Cancel" ]
                    ]
                ]

        maybeSelectedGroupPivot =
            Pivot.fromList (allGroups db)
                |> Maybe.map
                    (\p ->
                        case page.selectedGroupId of
                            Just gid ->
                                Pivot.withRollback (Pivot.firstWith (groupIdEq gid)) p

                            Nothing ->
                                p
                    )
    in
    div [ class "measure-wide center" ]
        [ viewPT
        , case maybeSelectedGroupPivot of
            Just pivot ->
                viewGroupPivot pivot

            Nothing ->
                viewEmptyGroups
        , case page.add of
            Nothing ->
                viewAddGroupButton

            Just add ->
                viewAddGroupInlineForm add
        ]


onEnter msg =
    Html.Events.on "keydown"
        (JD.field "key" JD.string
            |> JD.andThen
                (\actual ->
                    if actual == "Enter" then
                        JD.succeed msg

                    else
                        JD.fail "not enter"
                )
        )


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
            , ng "NA" []
            , ng "@Work" []
            , ng "@WF" []
            , ng "@SDMB" []
            , ng "@Mobile" []
            , ng "Trash" []
            , ng "#Prj1" []
            ]
    in
    dbFromList sampleData


main : Program Flags Model Msg
main =
    Browser.element
        { -- init = init
          init =
            init
                >> Tuple.mapFirst
                    (\m ->
                        { m
                            | db = sampleDb
                            , page =
                                PageGroups
                                    { add =
                                        Just
                                            { content = "Next Actions"
                                            }
                                            |> always Nothing
                                    , selectedGroupId = Nothing
                                    }
                        }
                    )
        , view = view

        --, view = always viewSample
        , update = update
        , subscriptions = subscriptions
        }
