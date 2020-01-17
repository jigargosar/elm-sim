module MirrorPuzzleV2.PuzzleGrid exposing
    ( Model
    , PuzzleGrid
    , fromString
    , initial
    , isSolved
    , levels
    , update
    , view
    )

import List.Extra
import Number2 as NT exposing (Float2, Int2)
import Playground exposing (..)
import Playground.CellTransform as CT exposing (CellTransform)
import Playground.Direction8 as Dir exposing (Direction8)
import Playground.Extra exposing (..)
import Playground.Grid as Grid exposing (Grid)
import PointFree exposing (is, mapEach, whenTrue)
import Set exposing (Set)


type Cell
    = Source
    | Destination
    | SourceWithMirror Direction8
    | Mirror Direction8
    | Empty


type alias Model =
    { grid : PuzzleGrid
    , mouseState : MouseState
    , mouseButton : MouseButton
    }


type alias PuzzleGrid =
    Grid Cell


mirror : Int -> Cell
mirror =
    Dir.fromInt >> Mirror


sourceWithMirror : Int -> Cell
sourceWithMirror =
    Dir.fromInt >> SourceWithMirror



{-
   SS       -> source
   DD       -> destination
   M<0-7>   -> mirror angle
   N<0-7>   -> source with mirror
   __       -> empty cell


   -- Later
   O<0-7> -> double mirror
   X -> wall
-}


levels : List String
levels =
    [ """
      __,__,__,__,__,__,__,__
      __,SS,M0,__,__,__,DD,__
      __,__,__,__,__,__,__,__
      """
    , """
      __,__,__,__,__,__,__,__
      __,N0,__,__,__,__,DD,__
      __,__,__,__,__,__,__,__
      """
    ]


encoded =
    """
    __,__,__,__,N5
    __,__,M7,__,__
    __,N1,__,DD,__
    __,SS,__,__,__
    DD,__,__,__,__
    """


fromGrid : PuzzleGrid -> Model
fromGrid grid =
    { grid = grid
    , mouseState = Up
    , mouseButton = ButtonNoChange
    }


fromString =
    gridFromString >> fromGrid


gridFromString : String -> PuzzleGrid
gridFromString str =
    let
        lines : List String
        lines =
            String.lines (String.trim str)
                |> List.reverse
    in
    case lines of
        first :: _ ->
            let
                height : Int
                height =
                    List.length lines

                width : Int
                width =
                    String.split "," first |> List.length

                updateFromCellString : Int2 -> String -> PuzzleGrid -> PuzzleGrid
                updateFromCellString pos cellStr =
                    let
                        ins =
                            Grid.insert pos

                        charToDir : Char -> Direction8
                        charToDir =
                            String.fromChar
                                >> String.toInt
                                >> Maybe.withDefault 0
                                >> Dir.fromInt
                    in
                    case String.trim cellStr of
                        "SS" ->
                            ins Source

                        "DD" ->
                            ins Destination

                        chars ->
                            case String.toList chars of
                                'M' :: [ char ] ->
                                    ins (Mirror (charToDir char))

                                'N' :: [ char ] ->
                                    ins (SourceWithMirror (charToDir char))

                                _ ->
                                    identity

                updateFromRowString : Int -> String -> PuzzleGrid -> PuzzleGrid
                updateFromRowString y rowString grid =
                    String.split "," rowString
                        |> List.Extra.indexedFoldl (\x cellStr -> updateFromCellString ( x, y ) cellStr) grid

                updateFromRowStrings : PuzzleGrid -> PuzzleGrid
                updateFromRowStrings grid =
                    List.Extra.indexedFoldl updateFromRowString grid lines
            in
            Grid.filled width height Empty
                |> updateFromRowStrings

        _ ->
            Grid.filled 0 0 Empty


initial : Model
initial =
    fromGrid initialGrid


initialGrid : PuzzleGrid
initialGrid =
    let
        insert =
            Grid.insert

        _ =
            Debug.log "e"
                (Grid.filled 5 5 Empty
                    |> insert ( 1, 2 ) (sourceWithMirror 1)
                    |> insert ( 2, 3 ) (mirror 7)
                    |> insert ( 3, 2 ) Destination
                    |> insert ( 4, 4 ) (sourceWithMirror -3)
                    |> insert ( 0, 0 ) Destination
                    |> insert ( 1, 1 ) (sourceWithMirror 1)
                    |> insert ( 1, 1 ) Source
                    |> is (gridFromString encoded)
                )
    in
    gridFromString encoded


update : Computer -> Model -> Model
update computer model =
    let
        mp =
            ( computer.mouse.x, computer.mouse.y )

        newMouseButton =
            case ( computer.mouse.down, model.mouseButton ) of
                ( True, ButtonNoChange ) ->
                    ButtonDown 0 mp mp

                ( True, ButtonDown elapsed start _ ) ->
                    ButtonDown (elapsed + 1) start mp

                ( True, ButtonUp _ _ _ ) ->
                    ButtonDown 0 mp mp

                ( False, ButtonNoChange ) ->
                    ButtonNoChange

                ( False, ButtonDown elapsed start _ ) ->
                    ButtonUp elapsed start mp

                ( False, ButtonUp _ _ _ ) ->
                    ButtonNoChange
    in
    updateHelp computer { model | mouseButton = newMouseButton }


updateHelp : Computer -> Model -> Model
updateHelp { screen, mouse } model =
    let
        grid =
            model.grid

        ct =
            initCellT screen grid
    in
    case model.mouseState of
        DownStart elapsed start ->
            let
                ( dx, dy ) =
                    NT.sub ( mouse.x, mouse.y ) start
                        |> mapEach abs

                noOp =
                    { model | mouseState = DownStart (elapsed + 1) start }

                startPos =
                    ct.fromView start

                dragStart dir =
                    { model | mouseState = Dragging startPos dir }

                ins a =
                    { model | grid = Grid.insert startPos a grid, mouseState = Up }
                        |> Debug.log "onup"
            in
            case ( mouse.down, dx > 2 || dy > 2 || elapsed > 60, Grid.get startPos grid ) of
                ( True, True, Just cell ) ->
                    case cell of
                        Mirror dir ->
                            dragStart dir

                        SourceWithMirror dir ->
                            dragStart dir

                        _ ->
                            noOp

                ( False, False, Just cell ) ->
                    case cell of
                        Mirror dir ->
                            ins (Mirror (Dir.rotate 1 dir))

                        SourceWithMirror dir ->
                            ins (SourceWithMirror (Dir.rotate 1 dir))

                        _ ->
                            noOp

                ( False, _, _ ) ->
                    { model | mouseState = Up }

                _ ->
                    noOp

        Dragging dragPos _ ->
            let
                dropPos =
                    ct.fromView ( mouse.x, mouse.y )
            in
            case ( mouse.down, Grid.get dragPos model.grid, Grid.get dropPos model.grid ) of
                ( False, Just dragCell, Just dropCell ) ->
                    { model
                        | mouseState = Up
                        , grid = dndGrid dragPos dragCell dropPos dropCell model.grid
                    }

                _ ->
                    model

        Up ->
            if mouse.down then
                { model | mouseState = DownStart 0 ( mouse.x, mouse.y ) }
                    |> Debug.log "ondown"

            else
                model


dndGrid : Grid.Pos -> Cell -> Grid.Pos -> Cell -> Grid Cell -> Grid Cell
dndGrid dragPos dragCell dropPos dropCell grid =
    let
        ( newDragCell, newDropCell ) =
            case ( dragCell, dropCell ) of
                ( SourceWithMirror dir, Empty ) ->
                    ( Source, Mirror dir )

                ( SourceWithMirror dir, Source ) ->
                    ( Source, SourceWithMirror dir )

                ( Mirror dir, Empty ) ->
                    ( Empty, Mirror dir )

                ( Mirror dir, Source ) ->
                    ( Empty, SourceWithMirror dir )

                _ ->
                    ( dragCell, dropCell )
    in
    grid |> Grid.insert dragPos newDragCell |> Grid.insert dropPos newDropCell


type MouseState
    = DownStart Int Float2
    | Dragging Int2 Direction8
    | Up


type MouseButton
    = ButtonDown Int Float2 Float2
    | ButtonUp Int Float2 Float2
    | ButtonNoChange


destinationPositions : PuzzleGrid -> Set Int2
destinationPositions =
    Grid.foldl (\pos cell -> whenTrue (cell == Destination) (Set.insert pos))
        Set.empty


isSolved : Model -> Bool
isSolved =
    .grid >> (\grid -> destinationPositions grid == litDestinationPositions grid)


litDestinationPositions : PuzzleGrid -> Set Int2
litDestinationPositions grid =
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
    List Int2


gridToLightPaths : PuzzleGrid -> List LightPath
gridToLightPaths grid =
    let
        accumLightPos : Direction8 -> Int2 -> List Int2 -> List Int2
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


initCellT : Screen -> PuzzleGrid -> CellTransform
initCellT screen grid =
    let
        viewD =
            NT.scale 0.8 ( screen.width, screen.height )
    in
    CT.fromViewD viewD (Grid.dimensions grid)


dragStartPosition : MouseButton -> Maybe Float2
dragStartPosition mouseButton =
    case mouseButton of
        ButtonDown elapsed start _ ->
            if elapsed > 60 then
                Just start

            else
                Nothing

        ButtonUp _ _ _ ->
            Nothing

        ButtonNoChange ->
            Nothing


view : Computer -> Model -> Shape
view { time, screen, mouse } model =
    let
        grid =
            model.grid

        ct =
            initCellT screen grid

        lightPathsShape =
            grid
                |> gridToLightPaths
                |> List.map (viewPath ct)
                |> group

        dimPos =
            case model.mouseState of
                Dragging int2 _ ->
                    Set.singleton int2

                _ ->
                    Set.empty
    in
    group
        [ gridCellShapes time ct dimPos grid |> group
        , lightPathsShape
        , case model.mouseState of
            Dragging _ dir ->
                mirrorShape ct.cellSize dir
                    |> scale 0.8
                    |> move mouse.x mouse.y

            _ ->
                noShape
        ]



-- Puzzle Ligh Path View


viewPath : CellTransform -> List Int2 -> Shape
viewPath ct =
    List.map ct.toView
        >> (\path -> List.map2 (line red 5) path (List.drop 1 path))
        >> group



-- Puzzle Cell View


gridCellShapes : Time -> CellTransform -> Set Int2 -> PuzzleGrid -> List Shape
gridCellShapes time ct dimPos grid =
    let
        litDest =
            litDestinationPositions grid

        toCellView ( pos, cell ) =
            cellShape time ct dimPos litDest pos cell
    in
    grid |> Grid.toList |> List.map toCellView


cellShape : Time -> CellTransform -> Set Int2 -> Set Int2 -> Int2 -> Cell -> Shape
cellShape time ct dimPos litDest pos cell =
    let
        bg : Shape
        bg =
            group
                [ rectangle black width width |> scale 0.95
                , rectangle white width width |> scale 0.9
                ]

        width =
            ct.cellSize
    in
    cellContentShapes time width litDest pos cell
        |> List.map (scale 0.8 >> whenTrue (Set.member pos dimPos) (fade 0.5))
        |> (::) bg
        |> group
        |> move2 (ct.toView pos)


cellContentShapes : Time -> Number -> Set comparable -> comparable -> Cell -> List Shape
cellContentShapes time width litDest pos cell =
    case cell of
        Source ->
            [ srcShape width ]

        Destination ->
            [ destinationShape time width (Set.member pos litDest) ]

        SourceWithMirror dir ->
            [ srcShape width, mirrorShape width dir ]

        Mirror dir ->
            [ mirrorShape width dir ]

        Empty ->
            []


srcShape : Number -> Shape
srcShape width =
    rectangle orange width width


mirrorShape : Float -> Direction8 -> Shape
mirrorShape width dir =
    group
        [ group [ oval green (width / 2) width |> moveLeft (width / 6) ]
            |> rotate (Dir.toDegrees dir)
        , circle lightPurple 10
        ]
        |> scale 0.9


destinationShape : Time -> Float -> Bool -> Shape
destinationShape time width isLit =
    circle blue (width / 2)
        |> whenTrue isLit (blink time)
