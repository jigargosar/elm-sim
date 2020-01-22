module MirrorPuzzleV3.Main exposing (main)

import Dict exposing (Dict)
import Dict2d
import Graph.Tree as Tree exposing (Tree)
import Html
import Number2 exposing (Int2)
import Playground.Direction8 as D exposing (Direction8)


main =
    lightForest
        |> List.map (Tree.levelOrder viewNode [] >> Html.div [])
        |> Html.div []


viewNode : Int2 -> b -> List (Html.Html msg) -> List (Html.Html msg)
viewNode position forest acc =
    Html.div [] [ Html.text (Debug.toString position) ] :: acc


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
        , [ Continue, Continue, Continue, Split [ D.left ] ]
        ]
        |> Tuple.second


type alias Seed =
    { position : Int2
    , directions : List Direction8
    }


initSeeds : ( Int2, El ) -> List Seed
initSeeds ( position, el ) =
    case el of
        Start dirs ->
            [ Seed position dirs ]

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

        nextSeedAt : Direction8 -> Maybe Seed
        nextSeedAt direction =
            let
                nextPosition =
                    D.stepPos direction seed.position
            in
            Dict.get nextPosition grid
                |> Maybe.andThen (getNextLightDirections direction)
                |> Maybe.map (Seed nextPosition)
    in
    seed.directions |> List.filterMap nextSeedAt


lightForest : List (Tree Int2)
lightForest =
    let
        next : Seed -> ( Int2, List Seed )
        next seed =
            ( seed.position, nextSeeds seed )
    in
    Dict.toList grid
        |> List.concatMap (initSeeds >> Tree.unfoldForest next)
