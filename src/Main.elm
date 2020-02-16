module Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (autofocus, style, tabindex)
import Json.Decode as JD
import List.Extra
import Random exposing (Seed)
import Set exposing (Set)
import String exposing (fromInt)
import Svg exposing (g, rect, svg, text_)
import Svg.Attributes exposing (class, fill, stroke)
import TypedSvg.Attributes exposing (transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, strokeWidth, width)
import TypedSvg.Types exposing (Transform(..))



-- MASKS AND TETRONS MODEL


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


jMask =
    Mask 3 [ ( 2, 0 ), ( 2, 1 ), ( 2, 2 ), ( 1, 2 ) ]


lMask =
    Mask 3 [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 1, 2 ) ]


tMask =
    Mask 3 [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 1, 1 ) ]


emptyMask =
    Mask 0 []


type TetronName
    = Line
    | S
    | Z
    | L
    | J
    | T


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

        L ->
            create lMask "orange"

        J ->
            create jMask "cyan"

        T ->
            create tMask "purple"



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
        , score : Int
        , seed : Seed
    }


activateNext : Board a -> Board a
activateNext model =
    let
        nextTetron =
            tetronFromName model.nextTetronName

        randomNext =
            Random.uniform Line [ S, Z, L, J, T ]

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
            |> updateScore
            |> clearAndShiftRows
            |> activateNext

    else
        { m | state = GameOver }


rangeN : Int -> List Int
rangeN n =
    List.range 0 (n - 1)


isRowFilled : Board a -> Int -> Bool
isRowFilled m y =
    let
        propEq func expected val =
            func val == expected

        is =
            (==)
    in
    Dict.keys m.grid
        |> List.Extra.count (propEq Tuple.second y)
        |> is m.width


updateScore : Board a -> Board a
updateScore m =
    let
        clearedLines =
            rangeN m.height |> List.Extra.count (isRowFilled m)
    in
    { m | score = m.score + (clearedLines ^ 2 * 100) }


clearAndShiftRows : Board a -> Board a
clearAndShiftRows =
    let
        clearAndShift : Int -> Dict Int2 String -> Dict Int2 String
        clearAndShift rn =
            let
                fm ( ( x, y ), v ) =
                    if y == rn then
                        Nothing

                    else if y < rn then
                        Just ( ( x, y + 1 ), v )

                    else
                        Just ( ( x, y ), v )
            in
            Dict.toList
                >> List.filterMap fm
                >> Dict.fromList

        findFirstFilledRow : Board a -> Maybe Int
        findFirstFilledRow m =
            rangeN m.height
                |> List.Extra.find (isRowFilled m)

        setGridIn : Board a -> Dict Int2 String -> Board a
        setGridIn m grid =
            { m | grid = grid }

        do m =
            case findFirstFilledRow m of
                Just rn ->
                    do
                        (clearAndShift rn m.grid
                            |> setGridIn m
                        )

                Nothing ->
                    m
    in
    do


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



-- Keyboard


type alias Keyboard =
    { keyDowns : Dict String Bool
    , keyUps : Set String
    , keys : Set String
    , prevKeys : Set String
    }


inKeyDowns : String -> Keyboard -> Bool
inKeyDowns string m =
    Dict.member string m.keyDowns


inKeys string m =
    Set.member string m.keys


inPrevKeys string m =
    Set.member string m.prevKeys


inJustDown : String -> Keyboard -> Bool
inJustDown k m =
    inKeys k m && not (inPrevKeys k m)


updateKeyboardOnKeyUp : String -> Keyboard -> Keyboard
updateKeyboardOnKeyUp k m =
    { m
        | keyUps = Set.insert k m.keyUps
        , keys = Set.remove k m.keys
    }


updateKeyboardOnKeyDown : String -> Bool -> Keyboard -> Keyboard
updateKeyboardOnKeyDown key repeat m =
    { m
        | keyDowns = Dict.insert key repeat m.keyDowns
        , keys = Set.insert key m.keys
    }


tickKeyboard : Keyboard -> Keyboard
tickKeyboard m =
    { m | keyDowns = Dict.empty, keyUps = Set.empty, prevKeys = m.keys }



-- MODEL


type Model
    = Model Keyboard Mem


type alias Mem =
    Board
        { fallTrigger : FallTrigger
        }


type alias FallTrigger =
    { ticks : Int, delay : Int }


type State
    = Running
    | Paused
    | GameOver


type alias Flags =
    ()


fillMockRows : Board a -> Board a
fillMockRows m =
    let
        fillR y =
            List.range 0 (m.width - 2) |> List.map (Tuple.pair >> (|>) y >> Tuple.pair >> (|>) "gray")

        grid =
            List.range 15 (m.height - 1)
                |> List.concatMap fillR
                |> Dict.fromList
    in
    { m | grid = grid }


initialMem : Mem
initialMem =
    { -- BOARD
      grid = Dict.empty
    , width = 10
    , height = 20
    , x = 4
    , y = -2
    , color = ""
    , activeMask = emptyMask
    , nextTetronName = Line
    , state = Running
    , score = 0
    , seed = Random.initialSeed 0

    -- OTHER
    , fallTrigger = { ticks = 0, delay = 20 }
    }
        |> fillMockRows
        |> activateNext


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        model : Model
        model =
            Model
                { -- KEYBOARD
                  keyDowns = Dict.empty
                , keyUps = Set.empty
                , keys = Set.empty
                , prevKeys = Set.empty
                }
                initialMem
    in
    ( model
    , Cmd.none
    )


type alias Input =
    { left : Bool
    , right : Bool
    , dx : Int
    , speedUp : Bool
    , rotate : Bool
    , pause : Bool
    }


toInput : Keyboard -> Input
toInput kb =
    let
        inKeyDowns_ k =
            inKeyDowns k kb

        left =
            inKeyDowns_ "ArrowLeft"

        right =
            inKeyDowns_ "ArrowRight"
    in
    { left = left
    , right = right
    , dx =
        case ( left, right ) of
            ( True, True ) ->
                0

            ( False, False ) ->
                0

            ( True, False ) ->
                -1

            ( False, True ) ->
                1
    , speedUp = inKeyDowns_ "ArrowDown"
    , rotate = inKeyDowns_ "ArrowUp"
    , pause = inJustDown " " kb
    }


updateMem : Keyboard -> State -> Mem -> Mem
updateMem kb state =
    let
        input =
            toInput kb

        updateRunning mem =
            let
                ( shouldFall, fallTrigger ) =
                    stepFallTrigger mem.fallTrigger
            in
            { mem | fallTrigger = fallTrigger }
                |> whenTrue (shouldFall || input.speedUp) moveActiveDown
                |> whenTrue input.rotate tryRotate
                |> whenTrue (input.dx /= 0) (tryShiftX input.dx)

        setPaused mem =
            { mem | state = Paused }

        setRunning mem =
            { mem | state = Running }

        nop =
            identity
    in
    case ( input.pause, state ) of
        ( True, Running ) ->
            setPaused

        ( False, Running ) ->
            updateRunning

        ( True, Paused ) ->
            setRunning

        ( False, Paused ) ->
            nop

        ( False, GameOver ) ->
            nop

        ( True, GameOver ) ->
            always initialMem


whenTrue bool func arg =
    if bool then
        func arg

    else
        arg


stepFallTrigger : FallTrigger -> ( Bool, FallTrigger )
stepFallTrigger fall =
    let
        newFall =
            { fall | ticks = fall.ticks + 1 }

        isTriggered =
            fall.delay <= 0 || modBy fall.delay fall.ticks == 0
    in
    ( isTriggered, newFall )



-- Update


type Msg
    = Tick
    | OnKeyDown String Bool
    | OnKeyUp String


update : Msg -> Model -> ( Model, Cmd Msg )
update message (Model kb mem) =
    case message of
        Tick ->
            ( Model (tickKeyboard kb) (updateMem kb mem.state mem)
            , Cmd.none
            )

        OnKeyDown key repeat ->
            ( Model (updateKeyboardOnKeyDown key repeat kb) mem
            , Cmd.none
            )

        OnKeyUp k ->
            ( Model (updateKeyboardOnKeyUp k kb) mem
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrame (always Tick)
        , Browser.Events.onKeyDown (JD.map2 OnKeyDown (JD.field "key" JD.string) (JD.field "repeat" JD.bool))
        , Browser.Events.onKeyUp (JD.map OnKeyUp (JD.field "key" JD.string))
        ]



-- View


view : Model -> Html Msg
view (Model _ m) =
    let
        cellWidth =
            30
    in
    div
        [ class "df-row w-100 h-100 centerX centerY p10"
        ]
        [ div [ class "df-row sp10" ]
            [ viewGrid cellWidth m.state m.width m.height (gridWithActiveMask m)
                |> wrapSvg
            , div
                [ class "df-col sp10"

                --, style "justify-content" "space-around"
                --, style "justify-content" "space-evenly"
                ]
                [ div [ class "fw-bold" ]
                    [ span [] [ text "Score: " ]
                    , text (fromInt m.score)
                    ]
                , viewNext cellWidth m.nextTetronName
                ]
            ]
        , viewShapesDemo cellWidth
            |> remove
        ]


viewNext : Float -> TetronName -> Html msg
viewNext cw tetronName =
    let
        { mask, color } =
            tetronFromName tetronName
    in
    viewMask2 cw 4 color mask
        |> wrapSvg


remove _ =
    text ""


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
    , L
    , J
    , T
    ]
        |> List.map (tetronFromName >> viewShapeRotations)
        |> div [ class "df-row sp10 items-center f-wrap" ]


applyN : Int -> (c -> c) -> c -> c
applyN n func val =
    List.range 0 (n - 1)
        |> List.foldl (always func) val


svgWrapperStyles =
    [ style "border" "1px dotted gray"
    , class "lh0"
    ]


wrapSvg s =
    div svgWrapperStyles [ s ]


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


viewMask2 cw maskWidth color (Mask _ list) =
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
