module Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Set exposing (Set)
import Svg exposing (g, rect, svg)
import Svg.Attributes exposing (fill, stroke)
import TypedSvg.Attributes exposing (transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, strokeWidth, width)
import TypedSvg.Types exposing (Transform(..))



-- Model


type alias Int2 =
    ( Int, Int )


type Mask
    = Mask Int (Set Int2)


maskToList (Mask _ set) =
    Set.toList set


maskContainsAny setB (Mask _ setA) =
    Set.intersect setA setB |> Set.isEmpty |> not


maskAny pred (Mask _ setA) =
    Set.toList setA |> List.any pred


translateMask dx dy (Mask w set) =
    Set.map (\( x, y ) -> ( x + dx, y + dy )) set
        |> Mask w


rotateMask (Mask w set) =
    Set.map (\( x, y ) -> ( y, w - 1 - x )) set
        |> Mask w


lineMask =
    Mask 4 (Set.fromList [ ( 0, 1 ), ( 1, 1 ), ( 2, 1 ), ( 3, 1 ) ])


sMask =
    Mask 3 (Set.fromList [ ( 1, 1 ), ( 2, 1 ), ( 0, 2 ), ( 1, 2 ) ])


zMask =
    Mask 3 (Set.fromList [ ( 0, 1 ), ( 1, 1 ), ( 1, 2 ), ( 2, 2 ) ])


type TetronName
    = Line
    | S
    | Z


type alias Tetron =
    { mask : Mask, color : String }


tetronFromName : TetronName -> Tetron
tetronFromName shape =
    let
        create =
            Tetron
    in
    case shape of
        Line ->
            create lineMask "red"

        S ->
            create sMask "blue"

        Z ->
            create zMask "green"


type alias Model =
    { grid : Dict Int2 String
    , width : Int
    , height : Int
    , x : Int
    , y : Int
    , active : Tetron
    , next : TetronName
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { grid =
            Dict.empty
                |> Dict.insert ( 0, 0 ) "blue"
                |> Dict.insert ( 1, 1 ) "red"
                |> Dict.insert ( 10 - 1, 20 - 1 ) "black"
                |> Dict.insert ( 10, 20 ) "black"
      , width = 10
      , height = 20
      , x = 4
      , y = -2
      , active = tetronFromName Line
      , next = Line
      }
        |> insertNext
        |> tick
        |> tick
        |> tick
    , Cmd.none
    )


insertNext model =
    { model
        | x = 4
        , y = -2
        , active = tetronFromName model.next
    }


tick model =
    moveActiveDown model


moveActiveDown m =
    let
        newMask =
            translateMask m.x (m.y + 1) m.active.mask
    in
    if
        maskContainsAny (Dict.keys m.grid |> Set.fromList) newMask
            || maskAny (\( _, y ) -> y >= m.height) newMask
    then
        { m | grid = activeGrid m }
            |> insertNext

    else
        { m | y = m.y + 1 }



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- View


activeGrid m =
    let
        isValid w h ( x, y ) =
            x >= 0 && x <= w && y >= 0 && y < h

        pairTo b a =
            ( a, b )
    in
    translateMask m.x m.y m.active.mask
        |> maskToList
        |> List.filter (isValid m.width m.height)
        |> List.map (pairTo m.active.color)
        |> Dict.fromList
        |> Dict.union m.grid


view : Model -> Html Msg
view m =
    let
        cellWidth =
            30
    in
    div [ class "df-row sp10 items-center" ]
        [ viewShapesDemo cellWidth
        , viewGrid cellWidth m.width m.height (activeGrid m)
            |> wrapSvg
        ]


viewShapesDemo cw =
    [ Line
    , S
    , Z
    ]
        |> List.map (tetronFromName >> viewShapeRotations cw)
        |> div [ class "df-row sp10 items-center" ]


viewShapeRotations cw { color, mask } =
    List.range 0 3
        |> List.map
            (\n ->
                applyN n rotateMask mask
                    |> viewMask cw color
                    |> wrapSvg
            )
        |> div [ class "df-col sp10" ]


applyN : Int -> (c -> c) -> c -> c
applyN n func val =
    List.range 0 (n - 1)
        |> List.foldl (always func) val


wrapSvg s =
    div
        [ style "border" "1px dotted gray"
        , class "lh0"
        ]
        [ s ]


viewMask cw color (Mask maskWidth set) =
    let
        w =
            toFloat maskWidth * cw

        square ( x, y ) =
            rect
                [ width cw
                , height cw
                , fill color
                , strokeWidth 1
                , stroke "white"
                , transform [ Translate (toFloat x * cw) (toFloat y * cw) ]
                ]
                []
    in
    svg [ viewBox 0 0 w w, width w, height w ]
        [ Set.toList set
            |> List.map square
            |> g []
        ]


viewGrid cw gridWidth gridHeight grid =
    let
        square ( x, y ) color =
            rect
                [ width cw
                , height cw
                , fill color
                , transform [ Translate (toFloat x * cw) (toFloat y * cw) ]
                ]
                []

        ( w, h ) =
            ( toFloat gridWidth * cw, toFloat gridHeight * cw )
    in
    svg [ viewBox 0 0 w h, width w, height h ]
        [ Dict.map square grid
            |> Dict.values
            |> g []
        ]



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
