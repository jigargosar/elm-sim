module MirrorPuzzleV2.Main exposing (main)

import List.Extra
import MirrorPuzzleV2.Box as Box exposing (Box)
import NumberTuple as NT
import Playground exposing (..)
import Playground.CellTransform as CT exposing (CellTransform)
import Playground.Direction8 as Dir exposing (Direction8)
import Playground.Extra exposing (..)
import Playground.Grid as Grid exposing (Pos)
import PointFree exposing (whenTrue)
import Set exposing (Set)



-- Puzzle Data Model


type Cell
    = Source
    | Destination
    | SourceWithMirror Direction8
    | Mirror Direction8
    | Empty


type alias Grid =
    Grid.Grid Cell


mirror : Int -> Cell
mirror =
    Dir.fromInt >> Mirror


sourceWithMirror : Int -> Cell
sourceWithMirror =
    Dir.fromInt >> SourceWithMirror


initialGrid : Grid
initialGrid =
    Grid.filled 5 5 Empty
        |> insert ( 1, 2 ) (sourceWithMirror 1)
        |> insert ( 2, 3 ) (mirror 7)
        |> insert ( 3, 2 ) Destination
        |> insert ( 4, 4 ) (sourceWithMirror -3)
        |> insert ( 0, 0 ) Destination
        |> insert ( 1, 1 ) (sourceWithMirror 1)
        |> insert ( 1, 1 ) Source


insert : ( Int, Int ) -> Cell -> Grid -> Grid
insert =
    Grid.insert


allDestinationPos : Grid -> Set Pos
allDestinationPos =
    Grid.foldl (\pos cell -> whenTrue (cell == Destination) (Set.insert pos))
        Set.empty


isSolved : Grid -> Bool
isSolved grid =
    allDestinationPos grid == computeLitDestinationPosSet grid


computeLitDestinationPosSet : Grid -> Set Pos
computeLitDestinationPosSet grid =
    gridToLightPaths grid
        |> List.filterMap List.head
        |> List.foldl
            (\pos ->
                if Grid.get pos grid == Just Destination then
                    Set.insert pos

                else
                    identity
            )
            Set.empty


type alias LightPath =
    List Pos


gridToLightPaths : Grid -> List LightPath
gridToLightPaths grid =
    let
        accumLightPos : Direction8 -> Pos -> List Pos -> List Pos
        accumLightPos dir pos acc =
            let
                nextPos =
                    Dir.stepPos dir pos

                accumInDir newDir =
                    accumLightPos newDir nextPos (nextPos :: acc)
            in
            case Grid.get nextPos grid of
                Nothing ->
                    acc

                Just cell ->
                    case cell of
                        Source ->
                            accumInDir dir

                        SourceWithMirror _ ->
                            acc

                        Destination ->
                            nextPos :: acc

                        Mirror direction ->
                            accumInDir direction

                        Empty ->
                            accumInDir dir
    in
    Grid.foldl
        (\pos cell ->
            case cell of
                SourceWithMirror dir ->
                    accumLightPos dir pos [ pos ]
                        |> (::)

                _ ->
                    identity
        )
        []
        grid



-- Puzzle Grid View


toBgShape : Number -> Shape
toBgShape cz =
    group
        [ rectangle black cz cz |> scale 0.95
        , rectangle white cz cz |> scale 0.9
        ]


sourceShape : Number -> Shape
sourceShape cz =
    rectangle orange cz cz


mirrorShape : Number -> Direction8 -> Shape
mirrorShape cz dir =
    group
        [ group [ oval green (cz / 2) cz |> moveLeft (cz / 6) ]
            |> rotate (Dir.toDegrees dir)
        , circle lightPurple 10
        ]
        |> scale 0.9


toCellShape : Number -> Cell -> Shape
toCellShape cz cell =
    case cell of
        Source ->
            sourceShape cz

        Mirror dir ->
            mirrorShape cz dir

        SourceWithMirror dir ->
            group
                [ sourceShape cz
                , mirrorShape cz dir
                ]

        Destination ->
            circle blue (cz / 2)

        Empty ->
            noShape


renderPath : CellTransform -> List Pos -> Shape
renderPath ct =
    List.map (CT.fromPos ct)
        >> (\path -> List.map2 (line red 5) path (List.drop 1 path))
        >> group


initCellTransform : Screen -> Grid -> CellTransform
initCellTransform screen grid =
    let
        ( gw, gh ) =
            Grid.dimensions grid

        cellSize =
            min (screen.width * 0.8 / toFloat gw) (screen.height * 0.8 / toFloat gh)

        cellD =
            ( cellSize, cellSize )
    in
    CT.init cellD grid


viewGrid : Computer -> Grid -> Shape
viewGrid { time, screen } grid =
    let
        ct =
            initCellTransform screen grid

        renderLightPaths =
            gridToLightPaths grid
                |> List.map (renderPath ct)
                |> group
    in
    group
        [ renderCells time ct grid
        , renderLightPaths
        ]


renderCells : Time -> CellTransform -> Grid -> Shape
renderCells time ct grid =
    let
        litDest =
            computeLitDestinationPosSet grid

        blinkIfLitDest pos =
            whenTrue (Set.member pos litDest) (blink time)

        cz =
            CT.width ct

        toCellShapeHelp pos cell =
            case cell of
                Empty ->
                    toBgShape cz

                _ ->
                    [ toBgShape cz
                    , toCellShape cz cell
                        |> blinkIfLitDest pos
                        |> scale 0.85
                    ]
                        |> group

        renderCell pos cell =
            toCellShapeHelp pos cell |> move2 (CT.fromPos ct pos)
    in
    grid
        |> Grid.map renderCell
        |> Grid.values
        |> group


move2 ( x, y ) =
    move x y



-- LevelSelect


type alias LevelButtons =
    { top : Number
    , list : List Box
    }


initLevelButtons : Screen -> Int -> LevelButtons
initLevelButtons screen count =
    let
        lh =
            (screen.height * 0.7) / toFloat count

        hScale =
            0.8

        top =
            (toFloat count * lh - lh) / 2

        toY : Int -> Number
        toY n =
            top - (toFloat n * lh)

        height =
            lh * hScale

        width =
            150
    in
    { top = top
    , list =
        List.range 0 (count - 1)
            |> List.map
                (\n ->
                    Box.withWH width height
                        |> Box.moveY (toY n)
                )
    }


levelButtonIdxFromMouse : Mouse -> LevelButtons -> Maybe Int
levelButtonIdxFromMouse mouse lbs =
    lbs.list
        |> List.Extra.findIndex (Box.containsXY mouse)


renderLevelButtons : Mouse -> LevelButtons -> Shape
renderLevelButtons mouse lbs =
    lbs.list
        |> List.indexedMap
            (\levelIdx rect ->
                renderButton
                    mouse
                    ("Level " ++ String.fromInt (levelIdx + 1))
                    rect
            )
        |> group


renderButton : Mouse -> String -> Box -> Shape
renderButton mouse text rect =
    buttonShape (Box.containsXY mouse rect)
        (Box.dimensions rect)
        text
        |> move2 (Box.center rect)


buttonShape : Bool -> ( Number, Number ) -> String -> Shape
buttonShape hover ( w, h ) text =
    let
        thickness =
            3
    in
    [ rectangle black w h
    , rectangle
        (if hover then
            lightPurple

         else
            white
        )
        (w - thickness)
        (h - thickness)
    , words black text
    ]
        |> group


viewLevelSelect : Mouse -> LevelButtons -> List Shape
viewLevelSelect mouse lbs =
    [ words black "Select Level"
        |> scale 1.5
        |> moveUp lbs.top
        |> moveUp 60
    , renderLevelButtons mouse lbs
    ]



-- Scene


type Scene
    = Intro
    | LevelSelect Int
    | PuzzleScreen Puzzle


type alias Puzzle =
    { grid : Grid }



-- Game


type alias Mem =
    { scene : Scene }


initialPuzzle =
    PuzzleScreen { grid = initialGrid }


initialLevelSelect =
    LevelSelect 10


init : Mem
init =
    { scene = initialPuzzle }


type alias Mouse =
    { x : Number
    , y : Number
    , dx : Number
    , dy : Number
    , downAt : NT.Float
    , onDown : Bool
    , onUp : Bool
    , onClick : Bool
    , down : Bool
    , click : Bool
    }


initialMouse : Mouse
initialMouse =
    { x = 0
    , y = 0
    , dx = 0
    , dy = 0
    , onDown = False
    , downAt = ( 0, 0 )
    , onUp = False
    , down = False
    , click = False
    , onClick = False
    }


updateMouse : Playground.Mouse -> Mouse -> Mouse
updateMouse { x, y, down, click } prev =
    let
        ( dx, dy ) =
            ( x - prev.x, y - prev.y )

        onDown =
            down && not prev.down
    in
    { x = x
    , y = y
    , dx = dx
    , dy = dy
    , onDown = onDown
    , downAt =
        if onDown then
            ( x, y )

        else
            prev.downAt
    , onUp = not down && prev.down
    , down = down
    , click = click
    , onClick =
        if click then
            NT.equalWithin 2 ( x, y ) prev.downAt

        else
            False
    }


type alias Computer =
    { mouse : Mouse
    , keyboard : Keyboard
    , screen : Screen
    , time : Time
    }


type EnhancedMem
    = EnhancedMem Mouse Mem


enhancedInit =
    EnhancedMem initialMouse init


enhancedUpdate : Playground.Computer -> EnhancedMem -> EnhancedMem
enhancedUpdate { mouse, keyboard, screen, time } (EnhancedMem eMouse mem) =
    let
        nextMouse =
            updateMouse mouse eMouse

        nextComp =
            { mouse = nextMouse
            , keyboard = keyboard
            , screen = screen
            , time = time
            }
    in
    update nextComp mem |> EnhancedMem nextMouse


enhancedView : Playground.Computer -> EnhancedMem -> List Shape
enhancedView { mouse, keyboard, screen, time } (EnhancedMem eMouse mem) =
    let
        computer =
            { mouse = eMouse
            , keyboard = keyboard
            , screen = screen
            , time = time
            }
    in
    view computer mem


update : Computer -> Mem -> Mem
update computer mem =
    let
        { mouse, screen } =
            computer

        onClick =
            mouse.onClick
    in
    case mem.scene of
        Intro ->
            if onClick then
                { mem | scene = initialLevelSelect }

            else
                mem

        LevelSelect levelCount ->
            let
                lbs =
                    initLevelButtons screen levelCount
            in
            case ( onClick, levelButtonIdxFromMouse mouse lbs ) of
                ( True, Just _ ) ->
                    { mem | scene = initialPuzzle }

                _ ->
                    mem

        PuzzleScreen puzzle ->
            if onClick then
                if Box.containsXY mouse (initBackButtonBox screen) then
                    { mem | scene = initialLevelSelect }

                else
                    { mem | scene = PuzzleScreen (updatePuzzle computer puzzle) }

            else
                mem


updatePuzzle : Computer -> Puzzle -> Puzzle
updatePuzzle { screen, mouse } model =
    let
        grid =
            model.grid

        ct =
            initCellTransform screen grid

        pos =
            CT.xyToPos ct mouse
    in
    case Grid.get pos grid of
        Just cell ->
            let
                ins a =
                    { model | grid = Grid.insert pos a grid }
            in
            case cell of
                Mirror dir ->
                    ins (Mirror (Dir.rotate 1 dir))

                SourceWithMirror dir ->
                    ins (SourceWithMirror (Dir.rotate 1 dir))

                _ ->
                    model

        Nothing ->
            model


viewPuzzle : Computer -> Puzzle -> List Shape
viewPuzzle computer { grid } =
    let
        { mouse, time, screen } =
            computer
    in
    [ viewGrid computer grid
    , if isSolved grid then
        words black "puzzle solved"
            |> moveY screen.top
            |> moveDown 50

      else
        noShape
    , renderButton mouse "Back" (initBackButtonBox screen)
    ]


initBackButtonBox : Screen -> Box
initBackButtonBox screen =
    Box.withWH 100 30
        |> Box.move ( screen.left, screen.top )
        |> Box.moveDown 50
        |> Box.moveRight 100


view : Computer -> Mem -> List Shape
view computer mem =
    case mem.scene of
        PuzzleScreen puzzle ->
            viewPuzzle computer puzzle

        Intro ->
            [ words black "Tap To Start" ]

        LevelSelect levelCount ->
            let
                lbs =
                    initLevelButtons computer.screen levelCount
            in
            viewLevelSelect computer.mouse lbs


main =
    game enhancedView enhancedUpdate enhancedInit
