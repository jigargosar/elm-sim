module Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Json.Decode as JD
import Random exposing (Seed)
import Set exposing (Set)
import String
import Svg exposing (g, rect, svg, text_)
import Svg.Attributes exposing (class, fill, stroke)
import TypedSvg.Attributes exposing (transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, strokeWidth, width)
import TypedSvg.Types exposing (Transform(..))



-- Tetrons Board Model


type alias Board a =
    { a
        | grid : Dict Int2 String
        , width : Int
        , height : Int
        , x : Int
        , y : Int
        , color : String
        , activeMask : Mask
        , nextTetronName : TetronName
        , state : State
        , seed : Seed
    }



-- Model


type alias Int2 =
    ( Int, Int )


type Mask
    = Mask Int (List Int2)


maskToList (Mask _ list) =
    list


shiftNum2 dx dy ( x, y ) =
    ( x + dx, y + dy )


translateMask dx dy (Mask w list) =
    List.map (shiftNum2 dx dy) list
        |> Mask w


rotateMask (Mask w list) =
    List.map (\( x, y ) -> ( y, w - 1 - x )) list
        |> Mask w


lineMask =
    Mask 4 [ ( 0, 1 ), ( 1, 1 ), ( 2, 1 ), ( 3, 1 ) ]


sMask =
    Mask 3 [ ( 1, 1 ), ( 2, 1 ), ( 0, 2 ), ( 1, 2 ) ]


zMask =
    Mask 3 [ ( 0, 1 ), ( 1, 1 ), ( 1, 2 ), ( 2, 2 ) ]


emptyMask =
    Mask 0 []


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
    , color : String
    , activeMask : Mask
    , nextTetronName : TetronName
    , fallTrigger : FallTrigger
    , keyDowns : Set String
    , state : State
    , seed : Seed
    }


type alias FallTrigger =
    { ticks : Int, delay : Int }


type State
    = Running
    | Paused
    | GameOver


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        model : Model
        model =
            { grid = Dict.empty
            , width = 10
            , height = 20
            , x = 4
            , y = -2
            , color = ""
            , activeMask = emptyMask
            , nextTetronName = Line
            , state = Running
            , seed = Random.initialSeed 0
            , fallTrigger = { ticks = 0, delay = 20 }
            , keyDowns = Set.empty
            }
    in
    ( model |> activateNext
    , Cmd.none
    )


activateNext : Board a -> Board a
activateNext model =
    let
        nextTetron =
            tetronFromName model.nextTetronName

        randomNext =
            Random.uniform Line [ S, Z ]

        ( next, seed ) =
            Random.step randomNext model.seed
    in
    { model
        | x = 3
        , y = -2
        , color = nextTetron.color
        , activeMask = nextTetron.mask
        , nextTetronName = next
        , seed = seed
    }


updateRunning : Model -> Model
updateRunning m =
    let
        leftPressed =
            keyDown "ArrowLeft" m

        rightPressed =
            keyDown "ArrowRight" m

        dx =
            case ( leftPressed, rightPressed ) of
                ( True, True ) ->
                    0

                ( False, False ) ->
                    0

                ( True, False ) ->
                    -1

                ( False, True ) ->
                    1

        ( shouldFall, fallTrigger ) =
            stepFallTrigger m.fallTrigger
    in
    { m
        | fallTrigger = fallTrigger
    }
        |> whenTrue (shouldFall || keyDown "ArrowDown" m) moveActiveDown
        |> whenTrue (keyDown "ArrowUp" m) tryRotate
        |> whenTrue (dx /= 0) (tryShiftX dx)


tick : Model -> Model
tick model =
    case model.state of
        Running ->
            updateRunning model

        GameOver ->
            model

        Paused ->
            model


whenTrue bool func arg =
    if bool then
        func arg

    else
        arg


keyDown : String -> Model -> Bool
keyDown string m =
    Set.member string m.keyDowns


tryRotate : Board a -> Board a
tryRotate m =
    let
        newMask =
            rotateMask m.activeMask

        newMaskPoints =
            newMask
                |> translateMask m.x m.y
                |> maskToList
    in
    if List.all (isValidMaskPosition m) newMaskPoints then
        { m | activeMask = newMask }

    else
        m


tryShiftX : Int -> Board a -> Board a
tryShiftX dx m =
    let
        newMaskPoints =
            m.activeMask
                |> translateMask (m.x + dx) m.y
                |> maskToList
    in
    if List.all (isValidMaskPosition m) newMaskPoints then
        { m | x = m.x + dx }

    else
        m


stepFallTrigger : FallTrigger -> ( Bool, FallTrigger )
stepFallTrigger fall =
    let
        newFall =
            { fall | ticks = fall.ticks + 1 }

        isTriggered =
            fall.delay <= 0 || modBy fall.delay fall.ticks == 0
    in
    ( isTriggered, newFall )


moveActiveDown : Board a -> Board a
moveActiveDown m =
    let
        nextMaskPoints =
            translateMask m.x (m.y + 1) m.activeMask
                |> maskToList

        currentMaskPoints =
            translateMask m.x m.y m.activeMask
                |> maskToList
    in
    if List.all (isValidMaskPosition m) nextMaskPoints then
        { m | y = m.y + 1 }

    else if List.all (isValidInsertPosition m) currentMaskPoints then
        { m | grid = gridWithActiveMask m }
            |> activateNext

    else
        { m | state = GameOver }


isValidMaskPosition : Board a -> Int2 -> Bool
isValidMaskPosition m p =
    let
        gridMember =
            Dict.member p m.grid

        withingBoundsIgnoringMinY ( x, y ) =
            x >= 0 && x < m.width && y < m.height
    in
    not gridMember && withingBoundsIgnoringMinY p


isValidInsertPosition : Board a -> Int2 -> Bool
isValidInsertPosition m p =
    let
        gridMember =
            Dict.member p m.grid

        withingBounds ( x, y ) =
            x >= 0 && x < m.width && y < m.height && y >= 0
    in
    not gridMember && withingBounds p


gridWithActiveMask : Board a -> Dict Int2 String
gridWithActiveMask m =
    let
        isValid w h ( x, y ) =
            x >= 0 && x <= w && y >= 0 && y < h

        pairTo b a =
            ( a, b )
    in
    translateMask m.x m.y m.activeMask
        |> maskToList
        |> List.filter (isValid m.width m.height)
        |> List.map (pairTo m.color)
        |> Dict.fromList
        |> Dict.union m.grid



-- Update


type Msg
    = Tick
    | OnKeyDown String Bool
    | OnKeyUp String


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Tick ->
            ( tick model
                |> resetKeys
            , Cmd.none
            )

        OnKeyDown k _ ->
            ( { model | keyDowns = Set.insert k model.keyDowns }, Cmd.none )

        OnKeyUp _ ->
            ( model, Cmd.none )


resetKeys : Model -> Model
resetKeys m =
    { m | keyDowns = Set.empty }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrame (always Tick)
        , Browser.Events.onKeyDown (JD.map2 OnKeyDown (JD.field "key" JD.string) (JD.field "repeat" JD.bool))
        , Browser.Events.onKeyUp (JD.map OnKeyUp (JD.field "key" JD.string))
        ]



-- View


view : Model -> Html Msg
view m =
    let
        cellWidth =
            30
    in
    div [ class "df-row sp10 items-center" ]
        [ div [ class "df-col sp10" ]
            [ viewGrid cellWidth m.state m.width m.height (gridWithActiveMask m)
                |> wrapSvg
            ]
        , viewShapesDemo cellWidth
        ]


viewShapesDemo : Float -> Html msg
viewShapesDemo cw =
    let
        viewShapeRotations { color, mask } =
            List.range 0 3
                |> List.map
                    (\n ->
                        applyN n rotateMask mask
                            |> viewMask cw color
                            |> wrapSvg
                    )
                |> div [ class "df-col sp10" ]
    in
    [ Line
    , S
    , Z
    ]
        |> List.map (tetronFromName >> viewShapeRotations)
        |> div [ class "df-row sp10 items-center" ]


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


viewMask cw color (Mask maskWidth list) =
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
        [ list
            |> List.map square
            |> g []
        ]


viewGrid cw state gridWidth gridHeight grid =
    let
        filledSquare sideWidth color transforms attrs =
            rect
                (width sideWidth
                    :: height sideWidth
                    :: fill color
                    :: transform (transforms ++ [ Translate (sideWidth * -0.5) (sideWidth * -0.5) ])
                    :: attrs
                )
                []

        filledRect width_ height_ color transforms attrs =
            rect
                (width width_
                    :: height height_
                    :: fill color
                    :: transform (transforms ++ [ Translate (width_ * -0.5) (height_ * -0.5) ])
                    :: attrs
                )
                []

        gridSquare ( x, y ) color =
            filledSquare cw
                color
                [ Translate (toFloat x * cw) (toFloat y * cw) ]
                []

        ( w, h ) =
            ( toFloat gridWidth * cw, toFloat gridHeight * cw )

        filledText string color attrs =
            text_
                (TypedSvg.Attributes.dominantBaseline TypedSvg.Types.DominantBaselineCentral
                    :: TypedSvg.Attributes.textAnchor TypedSvg.Types.AnchorMiddle
                    :: fill color
                    :: attrs
                )
                [ text string ]

        group transforms attrs =
            g (transform transforms :: attrs)

        groupGrid =
            group [ Translate ((cw - w) * 0.5) ((cw - h) * 0.5) ] []
    in
    canvas w
        h
        [ Dict.map gridSquare grid
            |> Dict.values
            |> groupGrid
        , case state of
            GameOver ->
                [ filledRect (w * 0.75) (w / 10) "rgba(166, 166, 166, .902)" [] []
                , filledText "GAME OVER" "" []
                ]
                    |> group [] []

            Running ->
                text ""

            Paused ->
                [ filledRect (w * 0.75) (w / 10) "rgba(166, 166, 166, .902)" [] []
                , filledText "Paused" "" []
                ]
                    |> group [] []
        ]


canvas w h =
    svg [ viewBoxCentered w h, width w, height h ]


viewBoxCentered width_ height_ =
    viewBox (width_ * -0.5) (height_ * -0.5) width_ height_



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
