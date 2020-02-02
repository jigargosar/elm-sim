module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html as H exposing (Html, div)
import Html.Attributes exposing (class, style)
import Json.Decode as JD exposing (Decoder)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Grid


type Grid
    = Grid Int (Dict ( Int, Int ) Cell)


type Cell
    = Num Int
    | Empty


initGrid : Int -> Grid
initGrid size =
    let
        dict =
            times size (\r -> times size (Tuple.pair r))
                |> List.concat
                |> List.indexedMap (\i rc -> ( rc, Num (i + 1) ))
                |> Dict.fromList
    in
    Dict.insert ( size - 1, size - 1 ) Empty dict
        |> Grid size


times : Int -> (Int -> b) -> List b
times n func =
    List.range 0 (n - 1) |> List.map func


gridToRows : Grid -> List (List Cell)
gridToRows (Grid size dict) =
    let
        getRow n =
            dict
                |> Dict.filter (\( rowIdx, _ ) _ -> rowIdx == n)
                |> Dict.values
    in
    times size getRow


swapEmptyInOppositeDirection : Direction -> Grid -> Maybe Grid
swapEmptyInOppositeDirection direction (Grid size dict) =
    let
        maybeEmptyPosition =
            Dict.filter (\_ cell -> cell == Empty) dict
                |> Dict.keys
                |> List.head

        getNextPosition emptyPos =
            nextPositionInDirection (oppositeDirection direction) emptyPos
    in
    maybeEmptyPosition
        |> Maybe.andThen
            (\emptyPos ->
                swapValues emptyPos (getNextPosition emptyPos) dict
            )
        |> Maybe.map (Grid size)


swapValues : comparable -> comparable -> Dict comparable a -> Maybe (Dict comparable a)
swapValues k1 k2 dict =
    let
        swapInsert v1 v2 =
            dict
                |> Dict.insert k1 v2
                |> Dict.insert k2 v1
    in
    Maybe.map2 swapInsert (Dict.get k1 dict) (Dict.get k2 dict)


type Direction
    = Up
    | Down
    | Left
    | Right


oppositeDirection : Direction -> Direction
oppositeDirection direction =
    case direction of
        Up ->
            Down

        Down ->
            Up

        Left ->
            Right

        Right ->
            Left


nextPositionInDirection : Direction -> ( number, number ) -> ( number, number )
nextPositionInDirection direction =
    case direction of
        Up ->
            Tuple.mapFirst dec

        Down ->
            Tuple.mapFirst inc

        Left ->
            Tuple.mapSecond dec

        Right ->
            Tuple.mapSecond inc


dec =
    (+) -1


inc =
    (+) 1



-- Model


type alias Model =
    Grid


init : () -> ( Grid, Cmd msg )
init _ =
    ( initGrid 4, Cmd.none )


type Msg
    = DirectionKeyDown Direction


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        DirectionKeyDown direction ->
            ( swapEmptyInOppositeDirection direction model
                |> Maybe.withDefault model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ directionKeyDecoder
        |> JD.map DirectionKeyDown
        |> Browser.Events.onKeyDown
    ]
        |> Sub.batch


directionKeyDecoder : Decoder Direction
directionKeyDecoder =
    JD.field "key" JD.string
        |> JD.andThen
            (\key ->
                case key of
                    "ArrowUp" ->
                        JD.succeed Up

                    "ArrowDown" ->
                        JD.succeed Down

                    "ArrowLeft" ->
                        JD.succeed Left

                    "ArrowRight" ->
                        JD.succeed Right

                    _ ->
                        JD.fail "not direction key"
            )



-- View


view : Model -> Html Msg
view model =
    let
        grid =
            model
    in
    div
        [ flexCenter
        , fixedFullscreen
        ]
        [ renderGlobalStyles
        , renderGrid grid
        ]


renderGrid : Grid -> Html msg
renderGrid grid =
    let
        renderRow =
            List.map renderCell
                >> div [ flex ]
    in
    gridToRows grid
        |> List.map renderRow
        |> div [ flex, flexColumn, cellBorder ]


renderCell : Cell -> Html msg
renderCell cell =
    case cell of
        Num num ->
            renderCellString (String.fromInt num)
                [ style "background-color" "gray"
                ]

        Empty ->
            renderCellString "" []


renderCellString : String -> List (H.Attribute msg) -> Html msg
renderCellString cellContent attrs =
    div
        ([ style "width" "200px"
         , style "height" "200px"
         , cellBorder
         , style "font-size" "80px"
         , style "font-family" "monospace"
         , flexCenter
         ]
            ++ attrs
        )
        [ H.text cellContent ]


cellBorder : H.Attribute msg
cellBorder =
    style "border" "1px solid black"



-- STYLES


renderGlobalStyles : Html msg
renderGlobalStyles =
    H.node "style" [] [ H.text globalStyles ]


globalStyles : String
globalStyles =
    """
        .flex-center{
            display: flex;
            align-items: center;
            justify-content: center;
        }

        .fixed-fullscreen {
            position: fixed;
            width: 100%;
            height: 100%;
            top: 0;
            left: 0;
        }
    """


flexCenter : H.Attribute msg
flexCenter =
    class "flex-center"


fixedFullscreen : H.Attribute msg
fixedFullscreen =
    class "fixed-fullscreen"


flex : H.Attribute msg
flex =
    style "display" "flex"


flexColumn : H.Attribute msg
flexColumn =
    style "flex-direction" "column"
