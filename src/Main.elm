module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Group as G exposing (Group)
import GroupId as GI exposing (GroupId)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (autofocus, class, value)
import Html.Events exposing (onClick, onInput)
import Item as I exposing (Item)
import ItemId as II exposing (ItemId)
import Json.Decode as JD
import Maybe.Extra
import Pivot exposing (Pivot)
import Random exposing (Generator, Seed)
import Random.Extra
import String.Extra
import Tuple exposing (mapBoth)



-- DB


type Db
    = Db DbRecord


type alias DbRecord =
    { groupDict : Dict String Group
    , itemDict : Dict String Item
    }


emptyDb : Db
emptyDb =
    Db
        { groupDict = Dict.empty
        , itemDict = Dict.empty
        }


dbFromList : List ( String, List String ) -> Generator Db
dbFromList list =
    let
        randomDbDicts : List ( String, List String ) -> Generator ( Dict String Group, Dict String Item )
        randomDbDicts =
            List.map
                (\( gt, itl ) ->
                    G.random gt
                        |> Random.andThen
                            (\g ->
                                List.map (I.random (G.id g)) itl
                                    |> Random.Extra.combine
                                    |> Random.map (Tuple.pair g)
                            )
                )
                >> Random.Extra.combine
                >> Random.map
                    (List.unzip
                        >> Tuple.mapSecond List.concat
                        >> mapBoth
                            (dictFromListByKey G.idString)
                            (dictFromListByKey (I.id >> II.toString))
                    )
    in
    randomDbDicts list
        |> Random.map (\( g, i ) -> Db { groupDict = g, itemDict = i })


dictFromListByKey func =
    List.map (\v -> ( func v, v )) >> Dict.fromList



--noinspection ElmUnusedSymbol


findGroup : GroupId -> Db -> Maybe Group
findGroup gid (Db db) =
    Dict.get (GI.toString gid) db.groupDict


allGroups : Db -> List Group
allGroups (Db db) =
    db.groupDict |> Dict.values


findItemsInGroup : GroupId -> Db -> List Item
findItemsInGroup gid (Db db) =
    Dict.values db.itemDict
        |> List.filter (I.groupIdEq gid)


dbAddNewGroup : String -> Db -> Generator Db
dbAddNewGroup groupTitle (Db db) =
    G.random groupTitle
        |> Random.map
            (\group ->
                let
                    gid =
                        G.id group
                in
                Db { db | groupDict = Dict.insert (GI.toString gid) group db.groupDict }
            )



-- Model


type Route
    = RouteGroups
    | RouteItems GroupId


type Page
    = PageGroups PageGroupsRecord
    | PageItems PageItemsRecord


type alias GAdd =
    { content : String
    }


type alias PageGroupsRecord =
    { add : Maybe GAdd
    , selectedGroupId : Maybe GroupId
    }


type alias IAdd =
    { content : String }


type alias PageItemsRecord =
    { groupId : GroupId
    , add : Maybe IAdd
    , selectedItemId : Maybe ItemId
    }


type alias Model =
    { db : Db
    , page : Page
    , seed : Seed
    }


initialModel : Model
initialModel =
    { db = emptyDb
    , page = PageGroups { add = Nothing, selectedGroupId = Nothing }
    , seed = Random.initialSeed 0
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
    | AddFormClicked
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
                    PageItems { groupId = groupId, add = Nothing, selectedItemId = Nothing }
    in
    pure { model | page = newPage }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RouteTo route ->
            switchRoute route model

        GotDB db ->
            pure { model | db = db }

        AddFormClicked ->
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
                    let
                        randomDb =
                            case page.add |> Maybe.andThen (.content >> String.trim >> String.Extra.nonBlank) of
                                Just string ->
                                    dbAddNewGroup string model.db

                                Nothing ->
                                    Random.constant model.db

                        ( newDb, newSeed ) =
                            Random.step randomDb model.seed
                    in
                    ( { model
                        | page =
                            let
                                newPage =
                                    { page | add = Nothing }
                            in
                            PageGroups newPage
                        , db = newDb
                        , seed = newSeed
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
                                                                Pivot.withRollback (Pivot.firstWith (G.idEq gid)) p

                                                            Nothing ->
                                                                p
                                                    )
                                                |> Maybe.map (Pivot.withRollback Pivot.goR)
                                                |> Maybe.map (Pivot.getC >> G.id)

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
                                                                Pivot.withRollback (Pivot.firstWith (G.idEq gid)) p

                                                            Nothing ->
                                                                p
                                                    )
                                                |> Maybe.map (Pivot.withRollback Pivot.goL)
                                                |> Maybe.map (Pivot.getC >> G.id)

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
                                                        Pivot.withRollback (Pivot.firstWith (G.idEq gid)) p

                                                    Nothing ->
                                                        p
                                            )
                                        |> Maybe.map (Pivot.getC >> G.id >> RouteItems)
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

        PageItems page ->
            --viewGroupItems model.db pr
            viewItemsPage model.db page


viewGroupsPage : Db -> PageGroupsRecord -> Html Msg
viewGroupsPage db page =
    let
        viewGroupPivot : Pivot Group -> Html Msg
        viewGroupPivot groups =
            let
                viewGT : Group -> Html msg
                viewGT g =
                    div [ class "pointer pv1 ph2 br2" ] [ text (G.title g) ]

                viewSGT : Group -> Html msg
                viewSGT g =
                    div
                        [ class "pointer pv1 ph2 br2"
                        , class "bg-blue white"
                        ]
                        [ text (G.title g) ]
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
                , onClick AddFormClicked
                ]
                [ text "Add List" ]

        viewAddGroupInlineForm : GAdd -> Html Msg
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
                                Pivot.withRollback (Pivot.firstWith (G.idEq gid)) p

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


viewItemsPage : Db -> PageItemsRecord -> Html Msg
viewItemsPage db page =
    let
        viewItemPivot : Pivot Item -> Html Msg
        viewItemPivot items =
            let
                viewGT : Item -> Html msg
                viewGT i =
                    div [ class "pointer pv1 ph2 br2" ] [ text (I.title i) ]

                viewSGT : Item -> Html msg
                viewSGT i =
                    div
                        [ class "pointer pv1 ph2 br2"
                        , class "bg-blue white"
                        ]
                        [ text (I.title i) ]
            in
            div []
                (Pivot.mapCS viewSGT viewGT items
                    |> Pivot.toList
                )

        viewPT =
            div [ class "pv2 ttu tracked" ] [ text "Items" ]

        viewEmptyItems : Html Msg
        viewEmptyItems =
            div [] [ text "empty" ]

        btnStyle1 =
            class "pointer bn ph2 pv1 f5 ttu bg-inherit blue"

        viewAddItemButton : Html Msg
        viewAddItemButton =
            button
                [ btnStyle1
                , onClick AddFormClicked
                ]
                [ text "Add Item" ]

        viewAddItemInlineForm : GAdd -> Html Msg
        viewAddItemInlineForm add =
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

        maybeSelectedItemPivot =
            Pivot.fromList (findItemsInGroup page.groupId db)
                |> Maybe.map
                    (\p ->
                        case page.selectedItemId of
                            Just iid ->
                                Pivot.withRollback (Pivot.firstWith (I.idEq iid)) p

                            Nothing ->
                                p
                    )
    in
    div [ class "measure-wide center" ]
        [ viewPT
        , case maybeSelectedItemPivot of
            Just pivot ->
                viewItemPivot pivot

            Nothing ->
                viewEmptyItems
        , case page.add of
            Nothing ->
                viewAddItemButton

            Just add ->
                viewAddItemInlineForm add
        ]



-- Main


sampleDb : Db
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
        |> Random.step
        |> (|>) (Random.initialSeed 0)
        |> Tuple.first


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
