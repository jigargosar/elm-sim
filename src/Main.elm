module Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Json.Decode as JD
import Random exposing (Seed)
import String
import Svg exposing (g, rect, svg, text_)
import Svg.Attributes exposing (class, fill, stroke)
import TypedSvg.Attributes exposing (transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, strokeWidth, width)
import TypedSvg.Types exposing (Transform(..))



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
    , active : Mask
    , next : TetronName
    , ticks : Int
    , fall : { ticks : Int, delay : Int }
    , leftPressed : Bool
    , rightPressed : Bool
    , upPressed : Bool
    , state : State
    , seed : Seed
    }


type State
    = Running
    | GameOver


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
      , color = ""
      , active = emptyMask
      , next = Line
      , ticks = 0
      , fall = { ticks = 0, delay = 0 }
      , leftPressed = False
      , rightPressed = False
      , upPressed = False
      , state = Running
      , seed = Random.initialSeed 0
      }
        |> activateNext
        |> tick
        |> tick
        |> tick
        |> tick
        |> tick
    , Cmd.none
    )


activateNext : Model -> Model
activateNext model =
    let
        nextTetron =
            tetronFromName model.next

        randomNext =
            Random.uniform Line [ S, Z ]

        ( next, seed ) =
            Random.step randomNext model.seed
    in
    { model
        | x = 3
        , y = -2
        , color = nextTetron.color
        , active = nextTetron.mask
        , next = next
        , seed = seed
    }


tick : Model -> Model
tick model =
    case model.state of
        Running ->
            tickFall model
                |> tickRotate
                |> tickShiftX

        GameOver ->
            model


tickRotate : Model -> Model
tickRotate m =
    if m.upPressed then
        tryRotate m

    else
        m


tickShiftX : Model -> Model
tickShiftX m =
    case ( m.leftPressed, m.rightPressed ) of
        ( True, True ) ->
            m

        ( False, False ) ->
            m

        ( True, False ) ->
            tryShiftX -1 m

        ( False, True ) ->
            tryShiftX 1 m


tryRotate : Model -> Model
tryRotate m =
    let
        newMask =
            rotateMask m.active

        newMaskPoints =
            newMask
                |> translateMask m.x m.y
                |> maskToList
    in
    if List.all (isValidMaskPosition m) newMaskPoints then
        { m | active = newMask }

    else
        m


tryShiftX : Int -> Model -> Model
tryShiftX dx m =
    let
        newMask =
            translateMask dx 0 m.active

        newMaskPoints =
            newMask
                |> translateMask m.x m.y
                |> maskToList
    in
    if List.all (isValidMaskPosition m) newMaskPoints then
        { m | active = newMask }

    else
        m


tickFall : Model -> Model
tickFall model =
    let
        fall =
            model.fall

        newFall =
            { fall | ticks = fall.ticks + 1 }
    in
    if fall.delay <= 0 || modBy fall.delay fall.ticks == 0 then
        { model | fall = newFall }
            |> moveActiveDown

    else
        { model | fall = newFall }


moveActiveDown : Model -> Model
moveActiveDown m =
    let
        nextMaskPoints =
            translateMask m.x (m.y + 1) m.active
                |> maskToList

        currentMaskPoints =
            translateMask m.x m.y m.active
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
    translateMask m.x m.y m.active
        |> maskToList
        |> List.filter (isValid m.width m.height)
        |> List.map (pairTo m.color)
        |> Dict.fromList
        |> Dict.union m.grid



-- Update


type Msg
    = Tick
    | OnArrowUp Bool
    | OnArrowLeft Bool
    | OnArrowRigh Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Tick ->
            ( { model | ticks = model.ticks + 1 }
                |> tick
            , Cmd.none
            )

        OnArrowUp isPressed ->
            ( { model | upPressed = isPressed }, Cmd.none )

        OnArrowLeft isPressed ->
            ( { model | leftPressed = isPressed }, Cmd.none )

        OnArrowRigh isPressed ->
            ( { model | rightPressed = isPressed }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrame (always Tick)
        , JD.oneOf
            [ key "ArrowUp" (OnArrowUp True)
            , key "ArrowLeft" (OnArrowLeft True)
            , key "ArrowRight" (OnArrowRigh True)
            ]
            |> Browser.Events.onKeyDown
        , JD.oneOf
            [ key "ArrowUp" (OnArrowUp False)
            , key "ArrowLeft" (OnArrowLeft False)
            , key "ArrowRight" (OnArrowRigh False)
            ]
            |> Browser.Events.onKeyUp
        ]


key expected msg =
    JD.field "key" JD.string
        |> JD.andThen
            (\actual ->
                if actual == expected then
                    JD.succeed msg

                else
                    JD.fail "unexpected"
            )



-- View


view : Model -> Html Msg
view m =
    let
        cellWidth =
            30
    in
    div [ class "df-row sp10 items-center" ]
        [ viewShapesDemo cellWidth
        , viewGrid cellWidth m.state m.width m.height (gridWithActiveMask m)
            |> wrapSvg
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
