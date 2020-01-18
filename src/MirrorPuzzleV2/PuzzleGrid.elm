module MirrorPuzzleV2.PuzzleGrid exposing
    ( Model
    , fromString
    , initial
    , isSolved
    , levels
    , update
    , view
    )

import List.Extra
import MirrorPuzzleV2.Mouse2 as Mouse2 exposing (Mouse2)
import Number2 as NT exposing (Float2, Int2)
import Playground exposing (..)
import Playground.CellTransform as CT exposing (CellTransform)
import Playground.Direction8 as Dir exposing (Direction8)
import Playground.Extra exposing (..)
import Playground.Grid as Grid
import PointFree exposing (is, whenTrue)
import Set exposing (Set)


type Cell
    = Source
    | Destination
    | SourceWithMirror Direction8
    | Mirror Direction8
    | Empty


type alias Model =
    { grid : Grid
    , mouse2 : Mouse2
    }


type alias Grid =
    Grid.Grid Cell


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


fromGrid : Grid -> Model
fromGrid grid =
    { grid = grid
    , mouse2 = Mouse2.initial
    }


fromString =
    gridFromString >> fromGrid


gridFromString : String -> Grid
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

                updateFromCellString : Int2 -> String -> Grid -> Grid
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

                updateFromRowString : Int -> String -> Grid -> Grid
                updateFromRowString y rowString grid =
                    String.split "," rowString
                        |> List.Extra.indexedFoldl (\x cellStr -> updateFromCellString ( x, y ) cellStr) grid

                updateFromRowStrings : Grid -> Grid
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


initialGrid : Grid
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
    updateHelp computer { model | mouse2 = Mouse2.update computer.mouse model.mouse2 }


updateHelp : Computer -> Model -> Model
updateHelp { screen } model =
    let
        grid =
            model.grid

        ct =
            initCellT screen grid
    in
    case Mouse2.event model.mouse2 of
        Mouse2.OnClick mp ->
            let
                pos =
                    ct.fromView mp

                ins a =
                    Grid.insert pos a grid

                newGrid =
                    case Grid.get pos model.grid of
                        Just (SourceWithMirror dir) ->
                            ins (SourceWithMirror (Dir.rotate 1 dir))

                        Just (Mirror dir) ->
                            ins (Mirror (Dir.rotate 1 dir))

                        _ ->
                            grid
            in
            { model | grid = newGrid }

        Mouse2.OnDrop start current ->
            let
                dragPos =
                    ct.fromView start

                dropPos =
                    ct.fromView current
            in
            { model
                | grid =
                    case ( Grid.get dragPos grid, Grid.get dropPos grid ) of
                        ( Just dragCell, Just dropCell ) ->
                            updateGridOnDrop dragPos dragCell dropPos dropCell grid

                        _ ->
                            grid
            }

        _ ->
            model


updateGridOnDrop : Grid.Pos -> Cell -> Grid.Pos -> Cell -> Grid -> Grid
updateGridOnDrop dragPos dragCell dropPos dropCell grid =
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


destinationPositions : Grid -> Set Int2
destinationPositions =
    Grid.foldl (\pos cell -> whenTrue (cell == Destination) (Set.insert pos))
        Set.empty


isSolved : Model -> Bool
isSolved =
    .grid >> (\grid -> destinationPositions grid == litDestinationPositions grid)


litDestinationPositions : Grid -> Set Int2
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


gridToLightPaths : Grid -> List LightPath
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


initCellT : Screen -> Grid -> CellTransform
initCellT screen grid =
    let
        viewD =
            NT.scale 0.8 ( screen.width, screen.height )
    in
    CT.fromViewD viewD (Grid.dimensions grid)


type alias DragView =
    { startCellPos : Int2
    , currentViewPos : Float2
    , mirrorDir : Direction8
    }


toDragView : CellTransform -> Model -> Maybe DragView
toDragView ct model =
    case Mouse2.event model.mouse2 of
        Mouse2.OnDrag start current ->
            let
                pos =
                    ct.fromView start

                mirrorDir : Maybe Direction8
                mirrorDir =
                    case Grid.get pos model.grid of
                        Just cell ->
                            case cell of
                                SourceWithMirror dir ->
                                    Just dir

                                Mirror dir ->
                                    Just dir

                                _ ->
                                    Nothing

                        Nothing ->
                            Nothing
            in
            case mirrorDir of
                Just dir ->
                    Just { startCellPos = pos, currentViewPos = current, mirrorDir = dir }

                Nothing ->
                    Nothing

        _ ->
            Nothing


view : Computer -> Model -> Shape
view { time, screen } model =
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
            Mouse2.dragStartPosition model.mouse2
                |> Maybe.andThen
                    (\mp ->
                        let
                            pos =
                                ct.fromView mp

                            bool =
                                case Grid.get pos model.grid of
                                    Just (Mirror _) ->
                                        True

                                    Just (SourceWithMirror _) ->
                                        True

                                    _ ->
                                        False
                        in
                        if bool then
                            Just pos

                        else
                            Nothing
                    )
                |> Maybe.map Set.singleton
                |> Maybe.withDefault Set.empty
    in
    group
        [ gridCellShapes time ct dimPos grid |> group
        , lightPathsShape
        , case Mouse2.event model.mouse2 of
            Mouse2.OnDrag start current ->
                case Grid.get (ct.fromView start) model.grid of
                    Just (Mirror dir) ->
                        mirrorShape ct.cellSize dir
                            |> scale 0.8
                            |> move2 current

                    Just (SourceWithMirror dir) ->
                        mirrorShape ct.cellSize dir
                            |> scale 0.8
                            |> move2 current

                    _ ->
                        noShape

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


gridCellShapes : Time -> CellTransform -> Set Int2 -> Grid -> List Shape
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
