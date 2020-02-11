module DataModels exposing (..)

import Dict exposing (Dict)
import Random exposing (Generator)
import String exposing (String, fromInt)


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
