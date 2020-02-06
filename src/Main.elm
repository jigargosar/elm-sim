module Main exposing (main)

-- Browser.Element Scaffold

import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import String exposing (String, fromFloat)
import Svg as S exposing (svg, text, text_)
import Svg.Attributes as SA exposing (dominantBaseline, fill, stroke, textAnchor)
import Task
import Tuple exposing (mapBoth)
import TypedSvg.Attributes as TA exposing (viewBox)
import TypedSvg.Types as T


type Instruction
    = Start Direction


type Direction
    = Up
    | Down
    | Left
    | Right


oppositeDir dir =
    case dir of
        Up ->
            Down

        Down ->
            Up

        Left ->
            Right

        Right ->
            Left


dirToDeg dir =
    case dir of
        Up ->
            -90

        Down ->
            90

        Left ->
            180

        Right ->
            0


dirToUnitVec dir =
    case dir of
        Up ->
            ( 0, -1 )

        Down ->
            ( 0, 1 )

        Left ->
            ( -1, 0 )

        Right ->
            ( 1, 0 )


type alias Int2 =
    ( Int, Int )


type alias Float2 =
    ( Float, Float )


type alias Board =
    { insDict : Dict Int2 Instruction
    , moveDict : Dict Int2 Direction
    , start : PosDir
    }


boardWidth =
    10


boardHeight =
    8


boardSize : Int2
boardSize =
    ( boardWidth, boardHeight )


int2 : Int -> Int -> Int2
int2 x y =
    ( x, y )


boardPositions : List Int2
boardPositions =
    List.range 0 (boardWidth - 1)
        |> List.concatMap (\x -> List.range 0 (boardHeight - 1) |> List.map (int2 x))


emptyBoard : Board
emptyBoard =
    { insDict = Dict.empty
    , moveDict =
        Dict.empty
            |> Dict.insert ( 1, 1 ) Down
            |> Dict.insert ( 1, 4 ) Right
            |> Dict.insert ( 6, 4 ) Left
            |> Dict.insert ( 6, 4 ) Up
            |> Dict.insert ( 6, 1 ) Left

    --Dict.empty
    --|> Dict.insert ( 2, 0 ) Down
    --|> Dict.insert ( 2, 1 ) Up
    --|> Dict.insert ( 2, 2 ) Left
    --|> Dict.insert ( 1, 2 ) Right
    , start = PosDir ( 4, 1 ) Left
    }


instructionAt : Int2 -> Board -> Maybe Instruction
instructionAt p board =
    if p == board.start.pos then
        Just (Start board.start.dir)

    else
        Dict.get p board.insDict


moveAt : Int2 -> Board -> Maybe Direction
moveAt p board =
    Dict.get p board.moveDict


instructionList : Board -> List ( Int2, Instruction )
instructionList board =
    boardPositions
        |> List.filterMap
            (\p ->
                instructionAt p board
                    |> Maybe.map (Tuple.pair p)
            )


moveList : Board -> List ( Int2, Direction )
moveList board =
    boardPositions
        |> List.filterMap
            (\p ->
                moveAt p board
                    |> Maybe.map (Tuple.pair p)
            )



-- Render Background


type Background
    = Light
    | Darker
    | Dark


classifyBackground : Int2 -> Background
classifyBackground ( x, y ) =
    if x == 4 || x == 5 then
        -- Center
        Dark

    else if (x < 4 && y < 4) || (x > 4 && y > 3) then
        -- Quad 1
        Light

    else
        -- Quad 2
        Darker


renderCellBackgroundTile : Float2 -> Float -> Int2 -> List (S.Svg msg)
renderCellBackgroundTile cellSize cellWidth p =
    let
        bgColor =
            case classifyBackground p of
                Light ->
                    "#383838"

                Dark ->
                    "#2e2e2e"

                Darker ->
                    "#242424"
    in
    [ rect bgColor
        cellSize
        [ SA.rx (fromFloat (cellWidth / 15))
        , transformRect cellSize [ scale 0.985 ]
        ]
    ]


renderBackgroundTileLayer : Float -> S.Svg msg
renderBackgroundTileLayer cellWidth =
    let
        cellSize =
            ( cellWidth, cellWidth )
    in
    boardPositions
        |> List.map (\p -> ( p, renderCellBackgroundTile cellSize cellWidth p ))
        |> gridLayout cellSize boardSize []



-- Render MoveArrow


renderMove : String -> Float -> Float -> Direction -> List (S.Svg msg)
renderMove color offset cellWidth direction =
    let
        radius =
            cellWidth / 14
    in
    [ triangle color
        radius
        [ stroke "white"
        , strokeWidth (radius / 20)
        , transform
            [ rotate (dirToDeg direction + 90)
            , case direction of
                Up ->
                    shift ( offset, 0 )

                Down ->
                    shift ( offset, 0 )

                Left ->
                    shift ( 0, offset )

                Right ->
                    shift ( 0, offset )
            , shift
                (dirToUnitVec direction
                    -- Equation to touch the cell edge in given direction
                    |> mapEach (mul (cellWidth / 2 - radius))
                )
            ]
        ]
    ]


renderMoveLayer : String -> Float -> Float -> Board -> S.Svg msg
renderMoveLayer color offset cellWidth board =
    let
        cellSize =
            ( cellWidth, cellWidth )
    in
    moveList board
        |> List.map (\( p, v ) -> ( p, renderMove color offset cellWidth v ))
        |> gridLayout cellSize boardSize []



-- RENDER MOVE PATH


type alias PosDir =
    { pos : Int2, dir : Direction }


movePosInDir dir ( x, y ) =
    dirToUnitVec dir |> (\( dx, dy ) -> ( x + dx, y + dy ))


nextPosDir : PosDir -> PosDir
nextPosDir ({ pos, dir } as m) =
    { m | pos = movePosInDir dir pos }


movePathIndices : Board -> List Int2
movePathIndices board =
    let
        getNextPosDirOrCurrent current =
            let
                next =
                    nextPosDir current
            in
            if isValid next.pos then
                case moveAt next.pos board of
                    Just nextDir ->
                        { next | dir = nextDir }

                    Nothing ->
                        getNextPosDirOrCurrent next

            else
                current

        getNextPosDir : PosDir -> Maybe PosDir
        getNextPosDir current =
            let
                next =
                    nextPosDir current
            in
            if isValid next.pos then
                case moveAt next.pos board of
                    Just nextDir ->
                        Just { next | dir = nextDir }

                    Nothing ->
                        Just (getNextPosDirOrCurrent next)

            else
                Nothing

        isValid ( x, y ) =
            x >= 0 && y >= 0 && x < boardWidth && y < boardHeight

        isOpposite a b =
            a.dir == oppositeDir b.dir

        buildMovePath current path journal =
            case getNextPosDir current of
                Just next ->
                    if isOpposite current next then
                        -- Immediate Loop Node
                        next.pos :: path

                    else if Dict.get next.pos journal == Just next.dir then
                        -- Cyclic path
                        next.pos :: path

                    else
                        buildMovePath next (next.pos :: path) (Dict.insert next.pos next.dir journal)

                Nothing ->
                    path

        startBuildingMovePath from =
            buildMovePath from [ from.pos ] (Dict.singleton from.pos from.dir)
    in
    startBuildingMovePath board.start


renderMovePath : String -> Float -> Board -> S.Svg msg
renderMovePath color cellWidth board =
    let
        path =
            movePathIndices board

        points =
            path
                |> List.reverse
                |> List.map (mapEach (toFloat >> mul cellWidth))
    in
    [ S.polyline
        [ fill "none"
        , TA.points points
        , stroke color
        , strokeWidth (cellWidth / 50)
        , TA.strokeLinejoin T.StrokeLinejoinRound
        ]
        []
    ]
        |> groupGridTransform ( cellWidth, cellWidth ) boardSize []



-- RENDER INSTRUCTIONS


renderInstruction : String -> Float -> Instruction -> List (S.Svg msg)
renderInstruction color cellWidth instruction =
    let
        radius =
            cellWidth / 6
    in
    case instruction of
        Start direction ->
            [ empty
            , triangle color
                radius
                [ stroke "white"
                , strokeWidth (radius / 30)
                , transform
                    [ rotate (dirToDeg direction + 90)
                    , shift (dirToUnitVec direction |> mapEach (mul (radius * 0.5)))
                    ]
                ]
            , circle color
                radius
                [ stroke "white"
                , strokeWidth (radius / 30)

                --, transform [ shift ( cellWidth / 30, 0 ) ]
                ]
            , words "white" "Start" [ transform [ scale (radius / 16 * 0.75) ] ]
            ]


renderInstructionLayer : String -> Float -> Board -> S.Svg msg
renderInstructionLayer color cellWidth board =
    let
        cellSize =
            ( cellWidth, cellWidth )
    in
    instructionList board
        |> List.map (\( p, c ) -> ( p, renderInstruction color cellWidth c ))
        |> gridLayout cellSize boardSize []


mul =
    (*)



-- RENDER BOARD


renderBoard : Float -> Board -> S.Svg msg
renderBoard cellWidth board =
    let
        shiftLayer factor =
            List.singleton
                >> group [ transform [ shift ( factor, factor ) ] ]

        _ =
            movePathIndices board |> Debug.log "debug"

        blue =
            "#1e90ff"

        red =
            "#d74d2e"
    in
    [ renderBackgroundTileLayer cellWidth
    , renderMovePath blue cellWidth board
        |> shiftLayer (cellWidth / 6)
    , renderMovePath red cellWidth board
        |> shiftLayer (-cellWidth / 6)
    , renderInstructionLayer blue cellWidth board
        |> shiftLayer (cellWidth / 6)
    , renderInstructionLayer red cellWidth board
        |> shiftLayer (-cellWidth / 6)
    , renderMoveLayer blue (cellWidth / 6) cellWidth board
    , renderMoveLayer red (-cellWidth / 6) cellWidth board
    , boardGridLayout cellWidth
        []
        [ ( board.start.pos, renderWaldo blue cellWidth ) ]
    ]
        |> group []


renderWaldo color cellWidth =
    [ ring color
        (cellWidth / 2)
        (cellWidth / 10)
        [ transform [ scale 0.985 ] ]
    ]


boardGridLayout cellWidth =
    gridLayout ( cellWidth, cellWidth ) boardSize


ring color radius thickness attrs =
    circle "none"
        (radius - (thickness / 2))
        (stroke color
            :: strokeWidth thickness
            :: attrs
        )



-- Model


type alias Model =
    { screenSize : ( Float, Float ) }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        size =
            ( 600, 600 )
    in
    ( { screenSize = size }
    , Browser.Dom.getViewport |> Task.perform GotViewport
    )



-- Update


type Msg
    = NoOp
    | GotViewport Browser.Dom.Viewport
    | OnResize Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        GotViewport { scene } ->
            ( { model | screenSize = ( scene.width, scene.height ) }, Cmd.none )

        OnResize _ _ ->
            ( model, Browser.Dom.getViewport |> Task.perform GotViewport )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize OnResize
        ]



-- View


view : Model -> Html Msg
view model =
    let
        ( sw, sh ) =
            model.screenSize

        cellWidth =
            min (sw / boardWidth) (sh / boardHeight)
    in
    canvas model.screenSize
        [ HA.style "background-color" "black" ]
        [ polyRect "dodgerblue" ( 100, 100 ) []
        , renderBoard cellWidth emptyBoard
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



-- Grid Layout


mapEach : (a -> x) -> ( a, a ) -> ( x, x )
mapEach f =
    mapBoth f f


groupGridTransform cellSize gridSize_ attrs =
    let
        ( cellWidth, cellHeight ) =
            cellSize

        ( gridWidth, gridHeight ) =
            mapEach toFloat gridSize_

        dx =
            (cellWidth - (gridWidth * cellWidth)) / 2

        dy =
            (cellHeight - (gridHeight * cellHeight)) / 2
    in
    group (transform [ shift ( dx, dy ) ] :: attrs)


gridLayout cellSize gridSize_ attrs =
    let
        transformCell ( idx, svgView ) =
            let
                ( x, y ) =
                    mapEach toFloat idx
            in
            svgView
                |> group [ transform [ shift ( x * cellWidth, y * cellHeight ) ] ]

        ( cellWidth, cellHeight ) =
            cellSize

        ( gridWidth, gridHeight ) =
            mapEach toFloat gridSize_

        dx =
            (cellWidth - (gridWidth * cellWidth)) / 2

        dy =
            (cellHeight - (gridHeight * cellHeight)) / 2
    in
    List.map transformCell
        >> group (transform [ shift ( dx, dy ) ] :: attrs)



-- SVG CANVAS LIB


canvas ( w, h ) attrs =
    let
        ( x, y ) =
            ( -w / 2, -h / 2 )
    in
    svg
        (viewBox x y w h
            :: geometricPrecision
            :: TA.imageRendering T.RenderingOptimizeQuality
            :: TA.textRendering T.TextRenderingOptimizeLegibility
            :: HA.style "position" "fixed"
            :: HA.style "top" "0"
            :: HA.style "left" "0"
            :: HA.style "width" "100%"
            :: HA.style "height" "100%"
            :: attrs
        )


geometricPrecision =
    TA.shapeRendering T.RenderGeometricPrecision


crispEdges =
    TA.shapeRendering T.RenderCrispEdges


strokeWidth =
    fromFloat >> SA.strokeWidth


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
    polyRect c ( w, w )


polyRect color ( width, height ) attrs =
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


polygon color points attrs =
    S.polygon
        (TA.points points
            :: fill color
            :: attrs
        )
        []


rect : String -> Float2 -> List (S.Attribute msg) -> S.Svg msg
rect color ( width, height ) attrs =
    let
        ( x, y ) =
            ( width / 2, height / 2 )
    in
    S.rect
        (SA.width (fromFloat width)
            :: SA.height (fromFloat height)
            :: fill color
            :: attrs
        )
        []


circle color r =
    ellipse color ( r, r )


ellipse color ( width, height ) attrs =
    S.ellipse
        (SA.rx (fromFloat width)
            :: SA.ry (fromFloat height)
            :: geometricPrecision
            :: fill color
            :: attrs
        )
        []


triangle color radius =
    ngon color 3 radius


ngon color sides radius attrs =
    S.polygon
        (SA.points (toNgonPoints 0 sides radius "")
            :: fill color
            :: attrs
        )
        []


toNgonPoints : Int -> Int -> Float -> String -> String
toNgonPoints i n radius string =
    if i == n then
        string

    else
        let
            a =
                turns (toFloat i / toFloat n - 0.25)

            x =
                radius * cos a

            y =
                radius * sin a
        in
        toNgonPoints (i + 1) n radius (string ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ " ")


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


rotate deg t =
    { t | deg = deg }


shift ( dx, dy ) t =
    { t | x = t.x + dx, y = t.y + dy }


transform =
    applyTransformsToIdentity
        >> transformToString
        >> SA.transform


applyTransformsToIdentity =
    List.foldl (<|) identityTransform


transformRect : Float2 -> List (Transform -> Transform) -> S.Attribute msg
transformRect ( w, h ) list =
    let
        t1 =
            applyTransformsToIdentity list
                |> transformToString

        t2 =
            applyTransformsToIdentity
                [ shift ( -w / 2, -h / 2 ) ]
                |> transformToString
    in
    SA.transform (t1 ++ " " ++ t2)


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


fade =
    fromFloat >> SA.opacity


empty =
    S.text ""
