module TodoistInterface exposing (main)

-- Browser.Element Scaffold

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Random exposing (Generator)
import String exposing (String, fromInt, isEmpty, trim)



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
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { projectDict = Dict.empty
      , itemDict = Dict.empty
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


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        GotItems items ->
            ( { model | itemDict = addItems items model.itemDict }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


view : Model -> Html Msg
view model =
    div []
        [ div [ class "fw-bold fz-large" ] [ text "Items" ]
        , div [] (List.map viewItem (getAllItems model.itemDict))
        ]


isBlank =
    trim >> isEmpty


viewItem : Item -> Html Msg
viewItem item =
    let
        ( displayTitle, colorClass ) =
            if isBlank item.title then
                ( "<empty>", "fg-gray" )

            else
                ( item.title, "fg-inherit" )
    in
    div [ class colorClass ] [ text displayTitle ]



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
