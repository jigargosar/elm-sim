port module TodoistInterface exposing (main)

-- Browser.Element Scaffold

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, input, text)
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


randomNewItem : Generator Item
randomNewItem =
    Random.map2 initItemWithIdAndTitle
        randomIdString
        randomSampleItemTitle


type alias UserProject =
    { id : String
    , title : String
    }


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
    = EditItem Item
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
        [ randomNewItem
            |> Random.list 10
            |> Random.generate GotItems
        ]
    )


addItems : List Item -> ItemDict -> ItemDict
addItems items itemDict =
    let
        insertItem item =
            Dict.insert item.id item
    in
    List.foldl insertItem itemDict items


getAllItems : ItemDict -> List Item
getAllItems =
    Dict.values



-- Update


type Msg
    = NoOp
    | GotItems (List Item)
    | EditItemClicked Item
    | OnEnter
    | OnInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        GotItems items ->
            ( { model | itemDict = addItems items model.itemDict }, Cmd.none )

        EditItemClicked item ->
            ( { model | edit = EditItem item }
            , Cmd.batch
                [ focusIdNextTick "item-editor"
                ]
            )

        OnEnter ->
            ( model, Cmd.none )

        OnInput string ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


view : Model -> Html Msg
view model =
    div [ class "df-column w600 mx-auto" ]
        [ div [ class "fw-bold fz-large ph5 pv10 " ] [ text "Items" ]
        , div [] (List.map (viewItemOrEditItem model.edit) (getAllItems model.itemDict))
        ]


isBlank =
    trim >> isEmpty


viewItemOrEditItem edit item =
    let
        viewDefault =
            viewItem item
    in
    case edit of
        EditItem editItem ->
            if editItem.id == item.id then
                viewEditItem editItem

            else
                viewDefault

        _ ->
            viewDefault


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
        , div [ class "p5 fg1", onClick (EditItemClicked item) ] [ text displayTitle ]
        ]


viewEditItem : Item -> Html Msg
viewEditItem item =
    div [ class "df-row" ]
        [ div [ class "p5" ] [ input [ type_ "checkbox", class "p5" ] [] ]
        , div [ class "p5 fg1" ]
            [ input
                [ hid "item-editor"
                , value item.title
                , onInput OnInput
                , onKey [ enter OnEnter ]
                ]
                []
            ]
        ]


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
                JD.fail "unexpected"
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
