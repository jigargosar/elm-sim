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
    | Input
    | Grab
    | Drop
    | Output


type Atom
    = Atom


type MoveInstruction
    = ChangeDirection Direction
    | NoChange


type Direction
    = Up
    | Down
    | Left
    | Right


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


type alias Instructions =
    Dict Int2 Instruction


type alias Move =
    Direction


type alias Moves =
    Dict Int2 Move


type alias Board =
    { width : Int
    , height : Int
    , instructions : Instructions
    , moves : Moves
    , start : PosDir
    }


type alias Waldo =
    { pd : PosDir, hasAtom : Bool }


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


setStartInstruction : Int -> Int -> Direction -> Board -> Board
setStartInstruction x y direction board =
    let
        start =
            PosDir ( x, y ) direction
    in
    { board | start = start }


setInstruction : Int -> Int -> Instruction -> Board -> Maybe Board
setInstruction x y instruction board =
    if isValidBoardLocation x y board then
        case instruction of
            Start direction ->
                Just (setStartInstruction x y direction board)

            _ ->
                Just (mapInstructions (Dict.insert ( x, y ) instruction) board)

    else
        Nothing


setInstructions : List ( Int2, Instruction ) -> Board -> Maybe Board
setInstructions list board =
    List.foldl
        (\( ( x, y ), instruction ) ->
            Maybe.andThen (setInstruction x y instruction)
        )
        (Just board)
        list


instructionAt : Int2 -> Board -> Maybe Instruction
instructionAt p board =
    if p == board.start.pos then
        Just (Start board.start.dir)

    else
        case Dict.get p board.instructions of
            Just (Start _) ->
                Nothing

            ins ->
                ins


moveInstructionAt : Int -> Int -> Board -> Maybe MoveInstruction
moveInstructionAt x y board =
    if isValidBoardLocation x y board then
        Dict.get ( x, y ) board.moves
            |> Maybe.map ChangeDirection
            |> Maybe.withDefault NoChange
            |> Just

    else
        Nothing


setMove : Int -> Int -> Move -> Board -> Maybe Board
setMove x y move board =
    if isValidBoardLocation x y board then
        Just (mapMoves (Dict.insert ( x, y ) move) board)

    else
        Nothing


setMoves : List ( Int2, Move ) -> Board -> Maybe Board
setMoves list board =
    List.foldl
        (\( ( x, y ), move ) ->
            Maybe.andThen (setMove x y move)
        )
        (Just board)
        list


isValidBoardLocation : Int -> Int -> Board -> Bool
isValidBoardLocation x y board =
    let
        isIndexValid idx len =
            idx >= 0 && idx < len
    in
    isIndexValid x board.width && isIndexValid y board.height


mapInstructions : (Instructions -> Instructions) -> Board -> Board
mapInstructions func board =
    { board | instructions = func board.instructions }


mapMoves : (Moves -> Moves) -> Board -> Board
mapMoves func board =
    { board | moves = func board.moves }


moveAt : Int2 -> Board -> Maybe Direction
moveAt p board =
    Dict.get p board.moves


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


emptyBoard : Board
emptyBoard =
    { width = boardWidth
    , height = boardHeight
    , instructions = Dict.empty
    , start = PosDir ( 4, 1 ) Left
    , moves = Dict.empty
    }


initialBoard : Board
initialBoard =
    emptyBoard
        |> setInstructions
            [ ( ( 5, 1 ), Start Right )
            , ( ( 3, 1 ), Input )
            , ( ( 1, 1 ), Grab )
            , ( ( 7, 3 ), Drop )
            , ( ( 6, 1 ), Output )
            ]
        |> Maybe.andThen
            (setMoves
                [ ( ( 1, 1 ), Down )
                , ( ( 1, 4 ), Right )
                , ( ( 7, 4 ), Left )
                , ( ( 7, 4 ), Up )
                , ( ( 7, 1 ), Left )
                ]
            )
        |> Maybe.withDefault emptyBoard



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
        [ stroke "black"
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


type alias Journal =
    Dict Int2 Direction


buildMovePathAllIndices : Board -> Int -> Int -> Direction -> Journal -> List Int2 -> List Int2
buildMovePathAllIndices board x y direction journal path =
    let
        ( dx, dy ) =
            dirToUnitVec direction

        newXY =
            ( x + dx, y + dy )

        ( newX, newY ) =
            newXY
    in
    case moveInstructionAt newX newY board of
        Nothing ->
            path

        Just moveInstruction ->
            let
                newDirection =
                    case moveInstruction of
                        NoChange ->
                            direction

                        ChangeDirection changeDirection ->
                            changeDirection

                newPath =
                    newXY :: path
            in
            if Dict.get newXY journal == Just newDirection then
                newPath

            else
                buildMovePathAllIndices board newX newY newDirection (Dict.insert newXY newDirection journal) newPath


movePathAllIndices : Board -> List Int2
movePathAllIndices board =
    let
        xy =
            board.start.pos

        ( x, y ) =
            xy

        direction =
            board.start.dir
    in
    buildMovePathAllIndices board x y direction (Dict.singleton xy direction) [ xy ]


renderMovePath : String -> Float -> Board -> S.Svg msg
renderMovePath color cellWidth board =
    let
        pathLong =
            movePathAllIndices board

        points =
            pathLong
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
            cellWidth / 5

        circleHelp =
            circle color
                radius
                [ stroke "black"
                , strokeWidth (radius / 30)
                ]

        wordsHelp string =
            words "white"
                string
                [ transform [ scale (radius / 16 * 0.5) ]
                ]
    in
    case instruction of
        Start direction ->
            [ empty
            , triangle color
                radius
                [ stroke "black"
                , strokeWidth (radius / 30)
                , transform
                    [ rotate (dirToDeg direction + 90)
                    , shift (dirToUnitVec direction |> mapEach (mul (radius * 0.5)))
                    ]
                ]
            , circleHelp
            , wordsHelp "START"
            ]

        Input ->
            [ circleHelp, wordsHelp "IN" ]

        Grab ->
            [ circleHelp, wordsHelp "GRAB" ]

        Drop ->
            [ circleHelp, wordsHelp "DROP" ]

        Output ->
            [ circleHelp, wordsHelp "OUT" ]


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


renderBoard : Float -> Board -> Waldo -> Dict Int2 Atom -> S.Svg msg
renderBoard cellWidth board waldo atomDict =
    let
        shiftLayer factor =
            List.singleton
                >> group [ transform [ shift ( factor, factor ) ] ]

        _ =
            1

        --movePathIndices board |> Debug.log "debug"
        blue =
            "#1e90ff"

        red =
            "#d74d2e"
    in
    [ renderBackgroundTileLayer cellWidth
    , renderAtomLayer cellWidth atomDict
    , renderWaldoLayer blue cellWidth waldo
    , renderWaldoLayer red cellWidth waldo
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
    ]
        |> group []


renderAtomLayer cellWidth atomDict =
    boardGridLayout cellWidth
        []
        (Dict.map (\_ _ -> renderAtom cellWidth) atomDict
            |> Dict.toList
        )


renderAtom cellWidth =
    [ circle "white"
        (cellWidth / 2 - cellWidth / 10)
        [ transform [ scale 0.985 ] ]
    , words "black" "H" [ transform [ scale (cellWidth / 16 / 3) ] ]
    ]


renderWaldoLayer color cellWidth waldo =
    boardGridLayout cellWidth
        []
        [ ( waldo.pd.pos, renderWaldo color cellWidth waldo ) ]


renderWaldo : String -> Float -> Waldo -> List (S.Svg msg)
renderWaldo color cellWidth waldo =
    (if waldo.hasAtom then
        renderAtom cellWidth

     else
        []
    )
        ++ [ ring color
                (cellWidth / 2)
                (cellWidth / 10)
                [ fade 0.8, transform [ scale 0.985 ] ]
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
    { screenSize : ( Float, Float )
    , board : Board
    , waldo : Waldo
    , atomDict : Dict Int2 Atom
    , elapsed : Int
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        size =
            ( 600, 600 )

        board =
            initialBoard
    in
    ( { screenSize = size
      , board = board
      , waldo = Waldo board.start False
      , atomDict = Dict.empty
      , elapsed = 0
      }
    , Browser.Dom.getViewport |> Task.perform GotViewport
    )



-- Update


type Msg
    = NoOp
    | GotViewport Browser.Dom.Viewport
    | OnResize Int Int
    | Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        GotViewport { scene } ->
            ( { model | screenSize = ( scene.width, scene.height ) }, Cmd.none )

        OnResize _ _ ->
            ( model, Browser.Dom.getViewport |> Task.perform GotViewport )

        Tick ->
            --( List.range 0 0 |> List.foldl (\_ -> stepWaldo) model, Cmd.none )
            if model.elapsed > 20 then
                ( { model | elapsed = 0 } |> stepWaldo model.board, Cmd.none )

            else
                ( { model | elapsed = model.elapsed + 1 }, Cmd.none )


stepWaldo :
    Board
    ->
        { a
            | atomDict : Dict Int2 Atom
            , waldo : Waldo
        }
    ->
        { a
            | atomDict : Dict Int2 Atom
            , waldo : Waldo
        }
stepWaldo board model =
    let
        stepWaldoPosDir waldo =
            let
                stepWaldoPosDirMaybe : PosDir -> Maybe PosDir
                stepWaldoPosDirMaybe current =
                    let
                        next =
                            nextPosDir current
                    in
                    if isValid next.pos then
                        case moveAt next.pos board of
                            Just nextDir ->
                                Just { next | dir = nextDir }

                            Nothing ->
                                Just next

                    else
                        Nothing

                isValid ( x, y ) =
                    x >= 0 && y >= 0 && x < boardWidth && y < boardHeight
            in
            stepWaldoPosDirMaybe waldo |> Maybe.withDefault waldo

        getNewWaldo waldo =
            { waldo | pd = stepWaldoPosDir waldo.pd }
    in
    { model
        | waldo = getNewWaldo model.waldo
    }
        |> executeWaldoInstruction board


executeWaldoInstruction board ({ waldo, atomDict } as model) =
    case instructionAt waldo.pd.pos board of
        Just ins ->
            case ins of
                Start _ ->
                    model

                Input ->
                    { model | atomDict = Dict.insert ( 1, 1 ) Atom atomDict }

                Grab ->
                    case atomDict |> Dict.get waldo.pd.pos of
                        Just _ ->
                            { model
                                | atomDict = Dict.remove waldo.pd.pos atomDict
                                , waldo = { waldo | hasAtom = True }
                            }

                        Nothing ->
                            model

                Drop ->
                    if waldo.hasAtom then
                        { model
                            | atomDict = Dict.insert waldo.pd.pos Atom atomDict
                            , waldo = { waldo | hasAtom = False }
                        }

                    else
                        model

                Output ->
                    { model
                        | atomDict = Dict.empty
                    }

        Nothing ->
            model


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize OnResize

        --, Browser.Events.onKeyDown (JD.succeed StepWaldo)
        , Browser.Events.onAnimationFrame (\_ -> Tick)
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
        , renderBoard cellWidth model.board model.waldo model.atomDict
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



--noinspection ElmUnusedSymbol


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



--noinspection ElmUnusedSymbol


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



--noinspection ElmUnusedSymbol


polygon color points attrs =
    S.polygon
        (TA.points points
            :: fill color
            :: attrs
        )
        []


rect : String -> Float2 -> List (S.Attribute msg) -> S.Svg msg
rect color ( width, height ) attrs =
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
    { t | s = t.s * n }


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
