port module TodoistInterface exposing (main)

-- Browser.Element Scaffold

import AllItemsListView
import Browser
import DataModels exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, button, input, option, select, text)
import Html.Attributes exposing (class, selected, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import Random exposing (Generator)
import String exposing (String, isEmpty, trim)
import ViewHelpers exposing (..)


port focusIdNextTick : String -> Cmd msg



-- Model


type alias Model =
    { projectDict : ProjectDict
    , itemDict : ItemDict
    , edit : Edit
    }


type Edit
    = EditItem Item (List Item)
    | NoEdit


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { projectDict = Dict.empty
      , itemDict = Dict.empty
      , edit = NoEdit
      }
    , Cmd.batch
        [ randomIdItemWithRandomSampleTitle
            |> Random.list 10
            |> Random.generate GotItems
        , [ "P1", "P2", "P3" ]
            |> List.map randomIdProjectWithTitle
            |> List.foldr (Random.map2 (::)) (Random.constant [])
            |> Random.generate GotProjects
        ]
    )


addItems : List Item -> ItemDict -> ItemDict
addItems =
    insertAllById


addProjects : List UserProject -> ProjectDict -> ProjectDict
addProjects =
    insertAllById


insertAllById list dict =
    let
        insertItem item =
            Dict.insert item.id item
    in
    List.foldl insertItem dict list


getAllItems : ItemDict -> List Item
getAllItems =
    Dict.values



-- Update


type Msg
    = NoOp
    | GotItems (List Item)
    | GotProjects (List UserProject)
    | OnEditItem Item
    | OnInputEnter
    | OnSave
    | OnCancel
    | OnInput String
    | OnSelectInput String


startEditItem ( m, c ) =
    let
        msg =
            getAllItems m.itemDict
                |> List.drop 4
                |> List.head
                |> Maybe.map OnEditItem
                |> Maybe.withDefault NoOp
    in
    andThenUpdate msg ( m, c )


andThenUpdate : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThenUpdate msg ( m, c ) =
    update msg m
        |> Tuple.mapSecond (\c2 -> Cmd.batch [ c, c2 ])


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        GotItems items ->
            ( { model | itemDict = addItems items model.itemDict }, Cmd.none )
                |> startEditItem

        GotProjects projects ->
            ( { model | projectDict = addProjects projects model.projectDict }, Cmd.none )

        OnEditItem item ->
            case model.edit of
                NoEdit ->
                    ( { model | edit = EditItem item (getAllItems model.itemDict) }
                    , Cmd.batch
                        [ focusIdNextTick "item-editor"
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        OnInputEnter ->
            ( handleSave model, Cmd.none )

        OnInput string ->
            ( { model | edit = handleInput string model.edit }, Cmd.none )

        OnSelectInput string ->
            ( { model | edit = handleSelectInput string model.edit }, Cmd.none )

        OnCancel ->
            ( { model | edit = NoEdit }, Cmd.none )

        OnSave ->
            ( handleSave model, Cmd.none )


handleSave model =
    case model.edit of
        NoEdit ->
            model

        EditItem item _ ->
            { model
                | itemDict = Dict.insert item.id item model.itemDict
                , edit = NoEdit
            }


handleInput string edit =
    case edit of
        NoEdit ->
            edit

        EditItem item itemList ->
            EditItem { item | title = string } itemList


projectIdFromString string =
    if isBlank string then
        InboxProjectId

    else
        UserProjectId (String.trim string)


handleSelectInput string edit =
    case edit of
        NoEdit ->
            edit

        EditItem item itemList ->
            EditItem { item | projectId = projectIdFromString string } itemList


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


view : Model -> Html Msg
view model =
    col [ class "w600 mx-auto" ]
        [ el [ class "fw-bold fz-large ph5 pv10 " ] (text "Items")
        , viewItemsList model
        ]


isBlank =
    trim >> isEmpty


findSplit : (a -> Bool) -> List a -> Maybe ( List a, a, List a )
findSplit pred list =
    let
        func item ( left, mc, right ) =
            case mc of
                Nothing ->
                    if pred item then
                        ( left, Just item, right )

                    else
                        ( item :: left, mc, right )

                Just _ ->
                    ( left, mc, item :: right )

        convertResult ( l, mc, r ) =
            Maybe.map (\c -> ( List.reverse l, c, List.reverse r )) mc
    in
    List.foldl func ( [], Nothing, [] ) list
        |> convertResult


viewItemsList model =
    case model.edit of
        EditItem editItem items ->
            let
                viewItemHelp item =
                    if item.id == editItem.id then
                        viewEditItem (model.projectDict |> Dict.values) editItem

                    else
                        viewItem item
            in
            col [] (List.map viewItemHelp items)

        NoEdit ->
            AllItemsListView.viewItemsList { onTitleClick = OnEditItem } (getAllItems model.itemDict)


viewItem : Item -> Html Msg
viewItem item =
    let
        ( displayTitle, colorClass ) =
            if isBlank item.title then
                ( "<empty>", "fg-gray" )

            else
                ( item.title, "fg-inherit" )
    in
    row [ class colorClass ]
        [ el [ class "p5" ] (input [ type_ "checkbox", class "p5" ] [])
        , el [ class "p5 fg1", onClick (OnEditItem item) ] (text displayTitle)
        ]


viewEditItem : List UserProject -> Item -> Html Msg
viewEditItem userProjects item =
    row [ sp5, p5 ]
        [ el [] (input [ type_ "checkbox" ] [])
        , col [ fill, sp10 ]
            [ row [ sp10 ]
                [ input
                    [ hid "item-editor"
                    , fill
                    , p5
                    , value item.title
                    , onInput OnInput
                    , onKey [ enter OnInputEnter ]
                    ]
                    []
                , selectProject item.projectId userProjects
                ]
            , row [ sp10 ]
                [ btn2 OnSave "SAVE"
                , btn2 OnCancel "CANCEL"
                ]
            ]
        ]


selectProject selectedProjectId userProjects =
    select [ fill, onInput OnSelectInput ]
        (option [ value "", selected (selectedProjectId == InboxProjectId) ] [ text "Inbox" ]
            :: List.map
                (\userProject ->
                    option
                        [ value userProject.id
                        , selected (selectedProjectId == UserProjectId userProject.id)
                        ]
                        [ text userProject.title ]
                )
                userProjects
        )


btn2 msg txt =
    button [ pv5, ph10, onClick msg ] [ text txt ]



-- VIEW HELPERS


enter : a -> JD.Decoder a
enter msg =
    keyEqDecoder "Enter" msg


keyEqDecoder : String -> b -> JD.Decoder b
keyEqDecoder keyName msg =
    JD.field "key" JD.string
        |> succeedWhenEq keyName msg


succeedWhenEq : a -> b -> JD.Decoder a -> JD.Decoder b
succeedWhenEq expected msg =
    JD.andThen
        (\actual ->
            if expected == actual then
                JD.succeed msg

            else
                JD.fail ("unexpected" |> Debug.log "fail")
        )


onKey decoders =
    Html.Events.on "keydown" (JD.oneOf decoders)


hid =
    Html.Attributes.id



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
