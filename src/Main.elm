module Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Random exposing (Seed)
import Set exposing (Set)
import String
import Svg exposing (g, rect, svg, text_)
import Svg.Attributes exposing (class, fill, stroke)
import TypedSvg.Attributes exposing (transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, strokeWidth, width)
import TypedSvg.Types exposing (Transform(..))



-- track how long key is held down, and trigger event at *delayed* rate.
-- also trigger first time key is pressed
-- KeyTrigger


type alias RepeatTrigger =
    { firstRepeatDelay : Int
    , repeatDelay : Int
    , state : TriggerState
    }


type TriggerState
    = NotTriggered
    | TriggeredOnce Int
    | TriggeredMoreThanOnce Int


initRepeatTrigger : Int -> Int -> RepeatTrigger
initRepeatTrigger firstRepeatDelay repeatDelay =
    RepeatTrigger firstRepeatDelay repeatDelay NotTriggered


defaultRepeatTrigger : RepeatTrigger
defaultRepeatTrigger =
    initRepeatTrigger 10 2


resetRepeatTrigger : RepeatTrigger -> RepeatTrigger
resetRepeatTrigger rt =
    { rt | state = NotTriggered }


stepRepeatTrigger : Bool -> RepeatTrigger -> ( Bool, RepeatTrigger )
stepRepeatTrigger isHeldDown kt =
    case ( isHeldDown, kt.state ) of
        ( True, NotTriggered ) ->
            ( True, { kt | state = TriggeredOnce 0 } )

        ( True, TriggeredOnce elapsed ) ->
            let
                didTrigger =
                    elapsed >= kt.firstRepeatDelay

                newState =
                    if didTrigger then
                        TriggeredMoreThanOnce 0

                    else
                        TriggeredOnce (elapsed + 1)
            in
            ( didTrigger, { kt | state = newState } )

        ( True, TriggeredMoreThanOnce elapsed ) ->
            let
                didTrigger =
                    elapsed >= kt.repeatDelay

                newState =
                    if didTrigger then
                        TriggeredMoreThanOnce 0

                    else
                        TriggeredMoreThanOnce (elapsed + 1)
            in
            ( didTrigger, { kt | state = newState } )

        ( False, NotTriggered ) ->
            ( False, kt )

        ( False, _ ) ->
            ( False, { kt | state = NotTriggered } )



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
    , ticks : Int
    , fallSpeed : { ticks : Int, delay : Int }
    , rotateClicked : Bool
    , leftClicked : Bool
    , rightClicked : Bool
    , speedUpClicked : Bool
    , rotateTrigger : RepeatTrigger
    , shiftXTrigger : RepeatTrigger
    , speedUpTrigger : RepeatTrigger
    , keys : Set String
    , prevKeys : Set String
    , state : State
    , seed : Seed
    }


type State
    = Running
    | Paused
    | GameOver


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { grid = Dict.empty
      , width = 10
      , height = 20
      , x = 4
      , y = -2
      , color = ""
      , activeMask = emptyMask
      , nextTetronName = Line
      , ticks = 0
      , fallSpeed = { ticks = 0, delay = 20 }
      , rotateTrigger = defaultRepeatTrigger
      , rotateClicked = False
      , leftClicked = False
      , rightClicked = False
      , speedUpClicked = False
      , shiftXTrigger = defaultRepeatTrigger
      , speedUpTrigger = initRepeatTrigger 10 1
      , keys = Set.empty
      , prevKeys = Set.empty
      , state = Running
      , seed = Random.initialSeed 0
      }
        |> activateNext
    , Cmd.none
    )


activateNext : Model -> Model
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
        , speedUpTrigger = resetRepeatTrigger model.speedUpTrigger
    }


tick : Model -> Model
tick model =
    case model.state of
        Running ->
            tickFall model
                |> tickRotate
                |> tickShiftX
                |> whenTriggered stepSpeedUpTrigger moveActiveDown
                |> when .rotateClicked tryRotate
                |> resetClicks

        GameOver ->
            model

        Paused ->
            model


resetClicks :
    { a | rotateClicked : Bool, leftClicked : Bool, rightClicked : Bool, speedUpClicked : Bool }
    -> { a | rotateClicked : Bool, leftClicked : Bool, rightClicked : Bool, speedUpClicked : Bool }
resetClicks m =
    { m
        | rotateClicked = False
        , leftClicked = False
        , rightClicked = False
        , speedUpClicked = False
    }


when pred true val =
    if pred val then
        true val

    else
        val


whenTriggered updateTrigger onTrigger model =
    let
        ( didTrigger, newModel ) =
            updateTrigger model
    in
    whenTrue didTrigger onTrigger newModel


stepSpeedUpTrigger : Model -> ( Bool, Model )
stepSpeedUpTrigger m =
    stepRepeatTrigger (keyDown "ArrowDown" m) m.speedUpTrigger
        |> Tuple.mapSecond (\t -> { m | speedUpTrigger = t })


tickRotate : Model -> Model
tickRotate model =
    let
        ( isTriggered, kt ) =
            stepRepeatTrigger (keyDown "ArrowUp" model) model.rotateTrigger

        newModel =
            { model | rotateTrigger = kt }
    in
    whenTrue isTriggered tryRotate newModel


whenTrue bool func arg =
    if bool then
        func arg

    else
        arg


keyDown : String -> Model -> Bool
keyDown string m =
    Set.member string m.keys


tickShiftX : Model -> Model
tickShiftX m =
    let
        leftPressed =
            keyDown "ArrowLeft" m

        rightPressed =
            keyDown "ArrowRight" m

        ( isTriggered, kt ) =
            stepRepeatTrigger (leftPressed || rightPressed) m.shiftXTrigger

        newModel =
            { m | shiftXTrigger = kt }
    in
    if isTriggered then
        case ( leftPressed, rightPressed ) of
            ( True, True ) ->
                newModel

            ( False, False ) ->
                newModel

            ( True, False ) ->
                tryShiftX -1 newModel

            ( False, True ) ->
                tryShiftX 1 newModel

    else
        newModel


tryRotate : Model -> Model
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


tryShiftX : Int -> Model -> Model
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


tickFall : Model -> Model
tickFall model =
    let
        fall =
            model.fallSpeed

        newFall =
            { fall | ticks = fall.ticks + 1 }
    in
    if fall.delay <= 0 || modBy fall.delay fall.ticks == 0 then
        { model | fallSpeed = newFall }
            |> moveActiveDown

    else
        { model | fallSpeed = newFall }


moveActiveDown : Model -> Model
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


isValidMaskPosition : Model -> Int2 -> Bool
isValidMaskPosition m p =
    let
        gridMember =
            Dict.member p m.grid

        withingBoundsIgnoringMinY ( x, y ) =
            x >= 0 && x < m.width && y < m.height
    in
    not gridMember && withingBoundsIgnoringMinY p


isValidInsertPosition : Model -> Int2 -> Bool
isValidInsertPosition m p =
    let
        gridMember =
            Dict.member p m.grid

        withingBounds ( x, y ) =
            x >= 0 && x < m.width && y < m.height && y >= 0
    in
    not gridMember && withingBounds p


gridWithActiveMask : Model -> Dict ( Int, Int ) String
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
    | OnKeyDown String
    | OnKeyUp String
    | RotateClicked
    | LeftClicked
    | RightClicked
    | SpeedUpClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Tick ->
            ( { model | ticks = model.ticks + 1 }
                |> tick
                |> (\m -> { m | prevKeys = m.keys })
            , Cmd.none
            )

        OnKeyDown k ->
            ( { model | keys = Set.insert k model.keys }, Cmd.none )

        OnKeyUp k ->
            ( { model | keys = Set.remove k model.keys }, Cmd.none )

        RotateClicked ->
            ( { model | rotateClicked = True }, Cmd.none )

        LeftClicked ->
            ( { model | leftClicked = True }, Cmd.none )

        RightClicked ->
            ( { model | rightClicked = True }, Cmd.none )

        SpeedUpClicked ->
            ( { model | speedUpClicked = True }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrame (always Tick)
        , Browser.Events.onKeyDown (JD.map OnKeyDown (JD.field "key" JD.string))
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
            , div [ class "df-row justify-center sp10" ]
                [ btn2 RotateClicked "Rotate"
                , btn1 "Left"
                , btn1 "Right"
                , btn1 "Down"
                ]
            ]
        , viewShapesDemo cellWidth
        ]


btn1 string =
    button [] [ text string ]


btn2 msg string =
    button [ onClick msg ] [ text string ]


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
