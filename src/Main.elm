module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import String exposing (fromInt)
import Svg as S exposing (svg, text, text_)
import Svg.Attributes as SA exposing (dominantBaseline, fill, textAnchor)
import Task
import Tuple exposing (mapBoth)
import TypedSvg.Attributes as TA exposing (viewBox)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Int2 =
    ( Int, Int )


type alias Float2 =
    ( Float, Float )


mapEach : (a -> x) -> ( a, a ) -> ( x, x )
mapEach f =
    mapBoth f f


toFloat2 : Int2 -> Float2
toFloat2 =
    mapEach toFloat


type Puzzle
    = Puzzle Int (Dict Int2 Cell)


type Cell
    = Num Int
    | Empty


initPuzzle : Int -> Puzzle
initPuzzle size =
    let
        toCell : Int -> Int -> Cell
        toCell x y =
            Num ((x * size) + y + 1)
    in
    initDict2d size size toCell
        |> Dict.insert ( size - 1, size - 1 ) Empty
        |> Puzzle size


initDict2d : Int -> Int -> (Int -> Int -> v) -> Dict ( Int, Int ) v
initDict2d w h func =
    foldLIndices2d w h (\x y -> Dict.insert ( x, y ) (func x y)) Dict.empty


foldLIndices2d : Int -> Int -> (Int -> Int -> c -> c) -> c -> c
foldLIndices2d w h func =
    foldLIndices w (\x -> foldLIndices h (func x))


indexRange : Int -> List Int
indexRange len =
    List.range 0 (len - 1)


foldLIndices : Int -> (Int -> c -> c) -> c -> c
foldLIndices len func acc =
    indexRange len |> List.foldl func acc


type alias Model =
    { screenD : Float2
    , puzzle : Puzzle
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { screenD = ( 600, 600 )
      , puzzle = initPuzzle 4
      }
    , Browser.Dom.getViewport |> Task.perform GotViewport
    )



-- Update


type Msg
    = OnResize Int Int
    | GotViewport Browser.Dom.Viewport


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        OnResize w h ->
            ( { model | screenD = ( w, h ) |> toFloat2 }, Cmd.none )

        GotViewport { scene } ->
            ( { model | screenD = ( scene.width, scene.height ) }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ Browser.Events.onResize OnResize ]
        |> Sub.batch



-- view


view : Model -> Html Msg
view model =
    let
        w =
            100
    in
    canvas model.screenD
        []
        [ viewPuzzle w model.puzzle
        ]


viewPuzzle : Float -> Puzzle -> S.Svg msg
viewPuzzle cellWidth (Puzzle size dict) =
    Dict.map (renderCell cellWidth) dict
        |> Dict.toList
        |> gridLayout ( cellWidth, cellWidth ) ( size, size )


gridLayout cellDimension gridDimension =
    let
        transformCell ( idx, svgView ) =
            let
                ( x, y ) =
                    mapEach toFloat idx
            in
            svgView
                |> group [ transform [ shift ( x * cellWidth, y * cellHeight ) ] ]

        ( cellWidth, cellHeight ) =
            cellDimension

        ( gridWidth, gridHeight ) =
            mapEach toFloat gridDimension

        dx =
            (cellWidth - (gridWidth * cellWidth)) / 2

        dy =
            (cellHeight - (gridHeight * cellHeight)) / 2
    in
    List.map transformCell
        >> group [ transform [ shift ( dx, dy ) ] ]


renderCell w _ cell =
    case cell of
        Num n ->
            [ square "dodgerblue" w []
            , words "black" (fromInt n) [ transform [ scale (w / 16 * 0.8) ] ]
            ]

        Empty ->
            []



-- SVG CANVAS LIB


canvas ( w, h ) attrs =
    let
        ( x, y ) =
            ( -w / 2, -h / 2 )
    in
    svg
        (viewBox x y w h
            :: SA.shapeRendering "optimizeSpeed"
            :: HA.style "position" "fixed"
            :: HA.style "top" "0"
            :: HA.style "left" "0"
            :: HA.style "width" "100%"
            :: HA.style "height" "100%"
            :: attrs
        )


group =
    S.g


words color string attrs =
    text_
        (textAnchor "middle"
            :: dominantBaseline "central"
            :: fill color
            :: attrs
        )
        [ text string ]


square c w =
    rect c w w


rect color width height attrs =
    let
        ( x, y ) =
            ( width / 2, height / 2 )
    in
    S.polygon
        (TA.points [ ( -x, -y ), ( x, -y ), ( x, y ), ( -x, y ) ]
            :: fill color
            :: attrs
        )
        []


type alias Transform =
    { x : Float
    , y : Float
    , s : Float
    , deg : Float
    }


identityTransform =
    Transform 0 0 1 0


scale n t =
    { t | s = n }


shift ( dx, dy ) t =
    { t | x = t.x + dx, y = t.y + dy }


transform =
    List.foldl (<|) identityTransform
        >> transformToString
        >> SA.transform


transformToString { x, y, s, deg } =
    let
        t name args =
            String.concat
                [ name
                , "("
                , String.join " " (List.map String.fromFloat args)
                , ")"
                ]
    in
    t "translate" [ x, y ]
        ++ " "
        ++ t "scale" [ s ]
        ++ " "
        ++ t "rotate" [ deg ]
