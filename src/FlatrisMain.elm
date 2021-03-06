module Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (autofocus, style, tabindex)
import Html.Events exposing (onBlur)
import Json.Decode as JD
import List exposing (map)
import List.Extra exposing (initialize)
import Maybe.Extra
import Random exposing (Seed)
import Set exposing (Set)
import String exposing (String, fromFloat, fromInt, join)
import Svg exposing (g, rect, svg, text_)
import Svg.Attributes exposing (class, fill)
import Tuple exposing (pair)



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
    map (shiftNum2 dx dy) list
        |> Mask w


rotateMask (Mask w list) =
    map (\( x, y ) -> ( y, w - 1 - x )) list
        |> Mask w


lineMask =
    Mask 4 [ ( 0, 1 ), ( 1, 1 ), ( 2, 1 ), ( 3, 1 ) ]


squareMask =
    Mask 2 [ ( 0, 0 ), ( 1, 0 ), ( 0, 1 ), ( 1, 1 ) ]


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
    | Square
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
            create lineMask "rgb(60, 199, 214)"

        Square ->
            create squareMask "rgb(251, 180, 20)"

        S ->
            create sMask "rgb(149, 196, 61)"

        Z ->
            create zMask "rgb(232, 65, 56)"

        L ->
            create lMask "rgb(237, 101, 47)"

        J ->
            create jMask "rgb(57, 147, 208)"

        T ->
            create tMask "rgb(176, 68, 151)"



-- Tetrons Board Model


type alias Grid a =
    { a
        | grid : Dict Int2 String
        , width : Int
        , height : Int
    }


type alias Board a =
    Grid
        { a
            | x : Int
            , y : Int
            , color : String
            , activeMask : Mask
            , nextTetronName : TetronName
            , state : State
            , score : Int
            , linesCleared : Int
            , seed : Seed
        }


flip func a b =
    func b a


pairTo =
    flip pair


fillMockRows : Board a -> Board a
fillMockRows m =
    let
        fillR y =
            initialize (m.width - 1) (pairTo y >> pairTo "gray")

        grid =
            rangeN m.height
                |> List.drop 15
                |> List.concatMap fillR
                |> Dict.fromList
    in
    { m | grid = grid }


activateNext : Board a -> Board a
activateNext model =
    let
        nextTetron =
            tetronFromName model.nextTetronName

        randomNext =
            Random.uniform Line [ Square, S, Z, L, J, T ]

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


rotate : Grid a -> Int -> Int -> Mask -> Maybe Mask
rotate m x y mask =
    let
        newMask =
            rotateMask mask

        newMaskPoints =
            newMask
                |> translateMask x y
                |> maskToList
    in
    if List.all (isValidMaskPosition m) newMaskPoints then
        Just newMask

    else
        Nothing


tryRotate : Board a -> Board a
tryRotate m =
    let
        maxDx =
            3

        shiftXRotate dx dxSign =
            if dx >= maxDx then
                Nothing

            else
                let
                    x =
                        m.x + (dx * dxSign)
                in
                case rotate m x m.y m.activeMask of
                    Just newMask ->
                        Just
                            { m
                                | activeMask = newMask
                                , x = x
                            }

                    Nothing ->
                        shiftXRotate (dx + 1) dxSign
    in
    shiftXRotate 0 1
        |> Maybe.Extra.orElseLazy (\_ -> shiftXRotate 0 -1)
        |> Maybe.withDefault m


tryShiftX : Int -> Board a -> Board a
tryShiftX dx =
    let
        shiftValid m =
            m.activeMask
                |> translateMask (m.x + dx) m.y
                |> maskToList
                |> List.all (isValidMaskPosition m)

        shift m =
            { m | x = m.x + dx }
    in
    when shiftValid shift


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
    Dict.keys m.grid
        |> List.Extra.count (propIs Tuple.second y)
        |> is m.width


countFilledRows m =
    rangeN m.height |> List.Extra.count (isRowFilled m)


scoreForFillingNRows : Int -> Int
scoreForFillingNRows n =
    n ^ 2 * 100


updateScore : Board a -> Board a
updateScore m =
    let
        newCleared =
            countFilledRows m
    in
    { m
        | score = m.score + scoreForFillingNRows (countFilledRows m)
        , linesCleared = m.linesCleared + newCleared
    }


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


isValidMaskPosition : Grid a -> Int2 -> Bool
isValidMaskPosition m p =
    let
        gridMember =
            Dict.member p m.grid

        withingBoundsIgnoringMinY ( x, y ) =
            x >= 0 && x < m.width && y < m.height
    in
    not gridMember && withingBoundsIgnoringMinY p


isValidInsertPosition : Grid a -> Int2 -> Bool
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
    in
    translateMask m.x m.y m.activeMask
        |> maskToList
        |> List.filter (isValid m.width m.height)
        |> map (pairTo m.color)
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
    , linesCleared = 0
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


setPaused mem =
    { mem | state = Paused }


setRunning mem =
    { mem | state = Running }


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
    | PauseOnBlur


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

        PauseOnBlur ->
            ( Model kb (when isRunning setPaused mem), Cmd.none )


when pred true val =
    if pred val then
        true val

    else
        val


propIs func expected val =
    func val == expected


is =
    (==)


isRunning =
    propIs .state Running


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

        gridCells =
            gridWithActiveMask m
                |> Dict.toList
    in
    div
        (class "df-row w-100 h-100"
            :: onBlur PauseOnBlur
            :: tabindex 0
            :: autofocus True
            :: style "outline" "none"
            :: []
        )
        [ div
            [ class "df-row sp20 p10"
            , style "margin" "auto"
            ]
            [ viewGrid cellWidth m.state m.width m.height gridCells
                |> wrap [ class "lh0", style "background-color" "rgb(236, 240, 241)" ]
            , viewPanel cellWidth m
            ]
        ]


viewPanel cw m =
    div
        [ class "df-col sp25"
        ]
        [ viewTitle "Elm Flatris"
        , div [ class "df-col" ]
            [ viewLabel "Score"
            , viewInt m.score
            ]
        , div [ class "df-col" ]
            [ viewLabel "Lines Cleared"
            , viewInt m.linesCleared
            ]
        , div [ class "df-col sp10" ]
            [ viewLabel "Next"
            , div [ style "align-self" "start" ]
                [ viewNext cw m.nextTetronName ]
            ]
        ]


primaryColor =
    "rgb(52,73,95)"


accentColor =
    "dodgerblue"


viewTitle txt =
    div
        [ style "font-size" "2.5rem"
        , style "line-height" "1"
        , style "color" primaryColor
        ]
        [ text txt ]


viewInt int =
    div
        [ style "font-size" "2.5rem"
        , style "line-height" "1"
        , style "color" accentColor
        ]
        [ text (fromInt int) ]


viewLabel txt =
    div
        [ style "font-size" "1rem"
        , style "line-height" "1"
        , style "color" "gray"
        ]
        [ text txt ]


viewNext : Float -> TetronName -> Html msg
viewNext cw tetronName =
    let
        { mask, color } =
            tetronFromName tetronName
    in
    viewMask cw 4 "rgb(192, 192, 192)" mask
        |> wrap [ class "lh0" ]


wrap a c =
    div a [ c ]


viewMask cw maskWidth color (Mask _ list) =
    let
        w =
            toFloat maskWidth * cw

        square ( x, y ) =
            rect
                [ width cw
                , height cw
                , fill color
                , tx [ move (toFloat x * cw) (toFloat y * cw) ]
                ]
                []
    in
    svg [ viewBox 0 0 w w, width w, height w ]
        [ list
            |> map square
            |> g []
        ]


viewGrid cellW state gridWidth gridHeight cellList =
    let
        gridSquare ( ( x, y ), color ) =
            filledSquare
                cellW
                color
                [ move (toFloat x * cellW) (toFloat y * cellW) ]
                []

        ( w, h ) =
            ( toFloat gridWidth * cellW, toFloat gridHeight * cellW )

        groupGridCells =
            group [ move ((cellW - w) * 0.5) ((cellW - h) * 0.5) ] []
    in
    canvas w h <|
        [ cellList
            |> List.map gridSquare
            |> groupGridCells
        , case state of
            GameOver ->
                overlayText cellW w h "GAME OVER"

            Running ->
                text ""

            Paused ->
                overlayText cellW w h "PAUSED"
        ]


overlayText cellW w h string =
    [ filledRect w h "rgba(255, 255, 255, .7)" [] []
    , filledText string
        primaryColor
        [ style "font-size" (fromFloat cellW ++ "px")
        , style "font-weight" "bold"
        ]
    ]
        |> group [] []


canvas w h =
    svg [ viewBoxCentered w h, width w, height h ]


viewBoxCentered w h =
    viewBox (w * -0.5) (h * -0.5) w h


filledSquare sideWidth color transforms attrs =
    rect
        (width sideWidth
            :: height sideWidth
            :: fill color
            :: tx (transforms ++ [ move (sideWidth * -0.5) (sideWidth * -0.5) ])
            :: attrs
        )
        []


filledRect w h color transforms attrs =
    rect
        (width w
            :: height h
            :: fill color
            :: tx (transforms ++ [ move (w * -0.5) (h * -0.5) ])
            :: attrs
        )
        []


filledText string color attrs =
    text_
        (dominantBaselineCentral
            :: textAnchorMiddle
            :: fill color
            :: attrs
        )
        [ text string ]


group transforms attrs =
    g (tx transforms :: attrs)



-- SVG ATTRIBUTES


floatAttr : (String.String -> a) -> Float -> a
floatAttr attr float =
    attr (fromFloat float)


width : Float -> Svg.Attribute msg
width =
    floatAttr Svg.Attributes.width


height : Float -> Svg.Attribute msg
height =
    floatAttr Svg.Attributes.height


viewBox : Float -> Float -> Float -> Float -> Svg.Attribute msg
viewBox minX minY vWidth vHeight =
    [ minX, minY, vWidth, vHeight ]
        |> List.map fromFloat
        |> String.join " "
        |> Svg.Attributes.viewBox


dominantBaselineCentral : Svg.Attribute msg
dominantBaselineCentral =
    Svg.Attributes.dominantBaseline "central"


textAnchorMiddle : Svg.Attribute msg
textAnchorMiddle =
    Svg.Attributes.textAnchor "middle"



-- TRANSFORMS


tx : List String -> Svg.Attribute msg
tx list =
    Svg.Attributes.transform <|
        join " " list


tx_ : String -> List Float -> String
tx_ name args =
    String.concat
        [ name
        , "("
        , String.join " " (List.map String.fromFloat args)
        , ")"
        ]


move : Float -> Float -> String
move x y =
    tx_ "translate" [ x, y ]



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
