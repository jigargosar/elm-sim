port module TodoistInterface exposing (main)

-- Browser.Element Scaffold

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, input, option, select, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import Random exposing (Generator)
import String exposing (String, fromInt, isEmpty, trim)


port focusIdNextTick : String -> Cmd msg



-- Model


type ProjectId
    = InboxProjectId
    | UserProjectId String


sampleItemTitles =
    [ "Remember The Milk!"
    , "Read Elm In Action"
    , "Study NeoVim lession 3.1"
    ]


randomSampleItemTitle =
    let
        sampleItemTitleAt idx =
            sampleItemTitles
                |> List.drop idx
                |> List.head
                |> Maybe.withDefault "Error: SampleItemTitle Not Found"
    in
    Random.int 0 (List.length sampleItemTitles - 1)
        |> Random.map sampleItemTitleAt


type alias Item =
    { id : String
    , title : String
    , completed : Bool
    , projectId : ProjectId
    }


initItemWithIdAndTitle : String -> String -> Item
initItemWithIdAndTitle id title =
    Item id title False InboxProjectId


randomIdItemWithRandomSampleTitle : Generator Item
randomIdItemWithRandomSampleTitle =
    Random.map2 initItemWithIdAndTitle
        randomIdString
        randomSampleItemTitle


type alias UserProject =
    { id : String
    , title : String
    }


initProjectWithIdAndTitle : String -> String -> UserProject
initProjectWithIdAndTitle id title =
    UserProject id title


randomIdProjectWithTitle : String -> Generator UserProject
randomIdProjectWithTitle title =
    randomIdString
        |> Random.map (\id -> initProjectWithIdAndTitle id title)


randomIdString : Generator String
randomIdString =
    Random.int 100 100000
        |> Random.map fromInt


type alias ItemDict =
    Dict String Item


type alias ProjectDict =
    Dict String UserProject


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


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        GotItems items ->
            ( { model | itemDict = addItems items model.itemDict }, Cmd.none )

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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


view : Model -> Html Msg
view model =
    div [ class "df-column w600 mx-auto" ]
        [ div [ class "fw-bold fz-large ph5 pv10 " ] [ text "Items" ]
        , viewItemsList model
        ]


isBlank =
    trim >> isEmpty


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
            div [] (List.map viewItemHelp items)

        NoEdit ->
            div [] (List.map viewItem (getAllItems model.itemDict))


viewItem : Item -> Html Msg
viewItem item =
    let
        ( displayTitle, colorClass ) =
            if isBlank item.title then
                ( "<empty>", "fg-gray" )

            else
                ( item.title, "fg-inherit" )
    in
    div [ class colorClass, class "df-row" ]
        [ div [ class "p5" ] [ input [ type_ "checkbox", class "p5" ] [] ]
        , div [ class "p5 fg1", onClick (OnEditItem item) ] [ text displayTitle ]
        ]


viewEditItem : List UserProject -> Item -> Html Msg
viewEditItem projects item =
    div [ class "df-row" ]
        [ div [ class "p5" ] [ input [ type_ "checkbox", class "p5" ] [] ]
        , div [ class "df-col fg1" ]
            [ div [ class "df-row" ]
                [ div [ class "p5 fg1 df-row" ]
                    [ input
                        [ hid "item-editor"
                        , class "fg1"
                        , value item.title
                        , onInput OnInput
                        , onKey [ enter OnInputEnter ]
                        ]
                        []
                    ]
                , div [ class "p5 fg1 df-row" ]
                    [ select [ class "fg1" ]
                        (option [] [ text "Inbox" ]
                            :: List.map viewProjectOption projects
                        )
                    ]
                ]
            , div [ class "df-row" ]
                [ div [ class "p5" ] [ button [ onClick OnSave ] [ text "save" ] ]
                , div [ class "p5" ] [ button [ onClick OnCancel ] [ text "cancel" ] ]
                ]
            ]
        ]


viewProjectOption project =
    option [] [ text project.title ]



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
