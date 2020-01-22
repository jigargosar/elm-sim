module MirrorPuzzleV3.Main exposing (main)

import Dict exposing (Dict)
import Dict2d
import Graph.Tree as Tree exposing (Tree)
import Html
import Html.Attributes
import Number2 exposing (Int2)
import Playground.Direction8 as D exposing (Direction8)
import Set exposing (Set)


main =
    viewForest 0 lightForest


viewForest : Int -> List (Tree a) -> Html.Html msg
viewForest level forest =
    Html.div []
        (List.filterMap (Tree.root >> Maybe.map (viewTree level)) forest)


viewTree : Int -> ( a, List (Tree b) ) -> Html.Html msg
viewTree level ( label, forest ) =
    Html.div [ Html.Attributes.class "ml3 pl3 bl b--red" ]
        [ Html.div [ Html.Attributes.class "pv3  " ] [ Html.text (Debug.toString label) ]
        , viewForest (level + 1) forest
        ]


type El
    = Start (List Direction8)
    | Continue
    | Split (List Direction8)
    | End


grid : Dict Int2 El
grid =
    Dict2d.fromListsWithDefault Continue
        [ [ Continue, Continue, Continue, Split [ D.left ] ]
        , [ Start [ D.right ], Continue, Continue, Split [ D.down, D.up ] ]
        , [ Split [ D.right ], Continue, Continue, Split [ D.left ] ]
        ]
        |> Tuple.second


type alias Seed =
    { visitedPositions : Set Int2
    , position : Int2
    , directions : List Direction8
    }


initSeeds : ( Int2, El ) -> List Seed
initSeeds ( position, el ) =
    case el of
        Start dirs ->
            [ Seed Set.empty position dirs ]

        _ ->
            []


nextSeeds : Seed -> List Seed
nextSeeds seed =
    let
        getNextLightDirections previousDirection el =
            case el of
                Split directions ->
                    Just directions

                End ->
                    Nothing

                Start _ ->
                    Just [ previousDirection ]

                Continue ->
                    Just [ previousDirection ]

        nextSeedAt : Set Int2 -> Direction8 -> Maybe Seed
        nextSeedAt visitedPositions direction =
            let
                nextPosition =
                    D.stepPos direction seed.position
            in
            Dict.get nextPosition grid
                |> Maybe.andThen (getNextLightDirections direction)
                |> Maybe.map (Seed visitedPositions nextPosition)
    in
    if Set.member seed.position seed.visitedPositions then
        []

    else
        seed.directions |> List.filterMap (nextSeedAt (Set.insert seed.position seed.visitedPositions))


lightForest : List (Tree Int2)
lightForest =
    let
        next : Seed -> ( Int2, List Seed )
        next seed =
            ( seed.position, nextSeeds seed )
    in
    Dict.toList grid |> List.concatMap (initSeeds >> Tree.unfoldForest next)
