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


type alias InstructionCell =
    { instruction : Instruction
    , moveInstruction : MoveInstruction
    }


type alias InstructionBoard =
    Dict Int2 InstructionCell


emptyInstructionBoard : InstructionBoard
emptyInstructionBoard =
    let
        ( w, h ) =
            boardSize
    in
    List.range 0 (w - 1)
        |> List.concatMap (\x -> List.range 0 (h - 1) |> List.map (\y -> ( x, y )))
        |> List.map (\xy -> ( xy, InstructionCell NoInstruction NoDirectionChange ))
        |> Dict.fromList


type Instruction
    = Input
    | Grab
    | Drop
    | Output
    | NoInstruction


type Atom
    = Atom


type MoveInstruction
    = ChangeDirection Direction
    | NoDirectionChange


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


type alias Board =
    { width : Int
    , height : Int
    , ib : InstructionBoard
    , start : { x : Int, y : Int, direction : Direction }
    }


type alias Waldo =
    { hasAtom : Bool
    , x : Int
    , y : Int
    , direction : Direction
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


setStartInstruction : Int -> Int -> Direction -> Board -> Board
setStartInstruction x y direction board =
    { board | start = { x = x, y = y, direction = direction } }


setInstruction2 : Int -> Int -> Instruction -> MoveInstruction -> Board -> Maybe Board
setInstruction2 x y instruction moveInstruction board =
    if isValidBoardLocation x y board then
        Just
            (mapInstructionBoard
                (Dict.insert ( x, y )
                    (InstructionCell instruction moveInstruction)
                )
                board
            )

    else
        Nothing


setInstruction2List : List ( Int2, Instruction, MoveInstruction ) -> Board -> Maybe Board
setInstruction2List list board =
    List.foldl
        (\( ( x, y ), instruction, moveInstruction ) ->
            Maybe.andThen (setInstruction2 x y instruction moveInstruction)
        )
        (Just board)
        list


instructionAt : Int -> Int -> Board -> Maybe Instruction
instructionAt x y board =
    let
        p =
            ( x, y )
    in
    if isValidBoardLocation x y board then
        Dict.get p board.ib
            |> Maybe.map .instruction

    else
        Nothing


moveInstructionAt : Int -> Int -> Board -> Maybe MoveInstruction
moveInstructionAt x y board =
    if isValidBoardLocation x y board then
        Dict.get ( x, y ) board.ib
            |> Maybe.map .moveInstruction

    else
        Nothing


mapInstructionBoard : (InstructionBoard -> InstructionBoard) -> Board -> Board
mapInstructionBoard func board =
    { board | ib = func board.ib }


isValidBoardLocation : Int -> Int -> Board -> Bool
isValidBoardLocation x y board =
    let
        isIndexValid idx len =
            idx >= 0 && idx < len
    in
    isIndexValid x board.width && isIndexValid y board.height


instructionList : Board -> List ( Int2, Instruction )
instructionList board =
    board.ib
        |> Dict.map (\_ -> .instruction)
        |> Dict.toList


moveInstructionList : Board -> List ( Int2, MoveInstruction )
moveInstructionList board =
    board.ib
        |> Dict.map (\_ -> .moveInstruction)
        |> Dict.toList


emptyBoard : Board
emptyBoard =
    { width = boardWidth
    , height = boardHeight
    , ib = emptyInstructionBoard
    , start = { x = 4, y = 1, direction = Left }
    }


initialBoard : Board
initialBoard =
    let
        nop =
            NoInstruction

        nod =
            NoDirectionChange

        cd =
            ChangeDirection
    in
    emptyBoard
        |> setStartInstruction 5 1 Right
        |> setInstruction2List
            [ ( ( 3, 1 ), Input, nod )
            , ( ( 1, 1 ), Grab, cd Down )
            , ( ( 1, 4 ), nop, cd Right )
            , ( ( 7, 4 ), nop, cd Left )
            , ( ( 7, 4 ), nop, cd Up )
            , ( ( 7, 3 ), Drop, nod )
            , ( ( 6, 1 ), Output, nod )
            , ( ( 7, 1 ), nop, cd Left )
            ]
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


renderMoveInstruction : String -> Float -> Float -> MoveInstruction -> List (S.Svg msg)
renderMoveInstruction color offset cellWidth moveInstruction =
    case moveInstruction of
        NoDirectionChange ->
            []

        ChangeDirection direction ->
            renderMove color offset cellWidth direction


renderMoveInstructionLayer : String -> Float -> Float -> Board -> S.Svg msg
renderMoveInstructionLayer color offset cellWidth board =
    let
        cellSize =
            ( cellWidth, cellWidth )
    in
    moveInstructionList board
        |> List.map (\( p, v ) -> ( p, renderMoveInstruction color offset cellWidth v ))
        |> gridLayout cellSize boardSize []



-- RENDER MOVE PATH


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

        recordInJournal newDirection =
            Dict.insert newXY newDirection journal

        alreadyVisited newDirection =
            Dict.get newXY journal == Just newDirection
    in
    case moveInstructionAt newX newY board of
        Nothing ->
            path

        Just moveInstruction ->
            let
                newDirection =
                    case moveInstruction of
                        NoDirectionChange ->
                            direction

                        ChangeDirection changeDirection ->
                            changeDirection

                newPath =
                    newXY :: path
            in
            if alreadyVisited newDirection then
                newPath

            else
                buildMovePathAllIndices
                    board
                    newX
                    newY
                    newDirection
                    (recordInJournal newDirection)
                    newPath


movePathAllIndices : Board -> List Int2
movePathAllIndices board =
    let
        { x, y, direction } =
            board.start

        initialJournal =
            Dict.singleton ( x, y ) direction

        initialPath =
            [ ( x, y ) ]
    in
    buildMovePathAllIndices board x y direction initialJournal initialPath
        |> List.reverse


renderMovePath : String -> Float -> Board -> S.Svg msg
renderMovePath color cellWidth board =
    let
        pathLong =
            movePathAllIndices board

        points =
            pathLong
                --|> List.reverse
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
        Input ->
            [ circleHelp, wordsHelp "IN" ]

        Grab ->
            [ circleHelp, wordsHelp "GRAB" ]

        Drop ->
            [ circleHelp, wordsHelp "DROP" ]

        Output ->
            [ circleHelp, wordsHelp "OUT" ]

        NoInstruction ->
            []


renderStartInstruction : String -> Float -> Direction -> List (S.Svg msg)
renderStartInstruction color cellWidth direction =
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


renderInstructionLayer : String -> Float -> Board -> S.Svg msg
renderInstructionLayer color cellWidth board =
    let
        cellSize =
            ( cellWidth, cellWidth )
    in
    instructionList board
        |> List.map (\( p, c ) -> ( p, renderInstruction color cellWidth c ))
        |> (::)
            ( ( board.start.x, board.start.y )
            , renderStartInstruction color cellWidth board.start.direction
            )
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
    , renderMoveInstructionLayer blue (cellWidth / 6) cellWidth board
    , renderMoveInstructionLayer red (-cellWidth / 6) cellWidth board
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


renderWaldoLayer : String -> Float -> Waldo -> S.Svg msg
renderWaldoLayer color cellWidth waldo =
    boardGridLayout cellWidth
        []
        [ ( ( waldo.x, waldo.y ), renderWaldo color cellWidth waldo ) ]


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
    , error : Bool
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

        start =
            board.start
    in
    ( { screenSize = size
      , board = board
      , waldo = { x = start.x, y = start.y, direction = start.direction, hasAtom = False }
      , atomDict = Dict.empty
      , error = True
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
            if model.error then
                ( model, Cmd.none )

            else
            --( List.range 0 0 |> List.foldl (\_ -> stepWaldo) model, Cmd.none )
            if
                model.elapsed > 20
            then
                ( { model | elapsed = 0 } |> stepWaldo model.board, Cmd.none )

            else
                ( { model | elapsed = model.elapsed + 1 }, Cmd.none )


stepWaldo board model =
    let
        moveWaldo : Waldo -> Maybe Waldo
        moveWaldo waldo =
            let
                { x, y } =
                    waldo
            in
            moveInstructionAt x y board
                |> Maybe.map
                    (\mi ->
                        let
                            direction =
                                case mi of
                                    ChangeDirection changeDirection ->
                                        changeDirection

                                    NoDirectionChange ->
                                        waldo.direction

                            ( dx, dy ) =
                                dirToUnitVec direction
                        in
                        { waldo | x = x + dx, y = y + dy, direction = direction }
                    )
    in
    moveWaldo model.waldo
        |> Maybe.map
            (\w ->
                { model
                    | waldo = w
                }
                    |> executeWaldoInstruction board
            )
        |> Maybe.withDefault { model | error = True }


executeWaldoInstruction board model =
    let
        { waldo, atomDict } =
            model

        waldoPos =
            ( waldo.x, waldo.y )
    in
    case instructionAt waldo.x waldo.y board of
        Just ins ->
            case ins of
                NoInstruction ->
                    model

                Input ->
                    { model | atomDict = Dict.insert ( 1, 1 ) Atom atomDict }

                Grab ->
                    case atomDict |> Dict.get waldoPos of
                        Just _ ->
                            { model
                                | atomDict = Dict.remove waldoPos atomDict
                                , waldo = { waldo | hasAtom = True }
                            }

                        Nothing ->
                            model

                Drop ->
                    if waldo.hasAtom then
                        { model
                            | atomDict = Dict.insert waldoPos Atom atomDict
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



--noinspection ElmUnusedSymbol


empty =
    S.text ""
