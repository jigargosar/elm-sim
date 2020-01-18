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
import Maybe.Extra
import MirrorPuzzleV2.Mouse2 as Mouse2 exposing (Mouse2)
import Number2 as NT exposing (Float2, Int2)
import Playground exposing (..)
import Playground.CellTransform as CT exposing (CellTransform)
import Playground.Direction8 as Dir exposing (Direction8)
import Playground.Extra exposing (..)
import Playground.Grid as Grid
import PointFree exposing (callWith, whenTrue)
import Set exposing (Set)



-- Model


type Model
    = Model Mouse2 Grid


initial : Model
initial =
    fromGrid initialGrid


fromString : String -> Model
fromString =
    decodeGrid >> fromGrid


fromGrid : Grid -> Model
fromGrid grid =
    Model Mouse2.initial grid


isSolved : Model -> Bool
isSolved (Model _ grid) =
    destinations grid == litDestinations grid


initCellT : Screen -> Grid -> CellTransform
initCellT screen grid =
    let
        viewD =
            NT.scale 0.8 ( screen.width, screen.height )
    in
    CT.fromViewD viewD (Grid.dimensions grid)



-- Update


update : Computer -> Model -> Model
update { mouse, screen } =
    mapMouse2 (Mouse2.update mouse)
        >> mapGrid (updateGrid screen)


mapMouse2 : (Mouse2 -> Mouse2) -> Model -> Model
mapMouse2 func (Model mouse2 grid) =
    Model (func mouse2) grid


mapGrid : (Mouse2 -> Grid -> Grid) -> Model -> Model
mapGrid func (Model mouse2 grid) =
    Model mouse2 (func mouse2 grid)



-- View


view : Computer -> Model -> Shape
view { time, screen } (Model mouse2 grid) =
    let
        ct =
            initCellT screen grid
    in
    viewGrid time mouse2 ct grid



-- Grid Model


type alias Grid =
    Grid.Grid Cell


type Cell
    = Source
    | Destination
    | SourceWithMirror Direction8
    | Mirror Direction8
    | Empty


initialGrid : Grid
initialGrid =
    decodeGrid encoded



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


decodeGrid : String -> Grid
decodeGrid str =
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


destinations : Grid -> Set Int2
destinations =
    Grid.foldl (\pos cell -> whenTrue (cell == Destination) (Set.insert pos))
        Set.empty


getMirrorDirection : Int2 -> Grid -> Maybe Direction8
getMirrorDirection pos grid =
    case Grid.get pos grid of
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


litDestinations : Grid -> Set Int2
litDestinations grid =
    lightPaths grid
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


lightPaths : Grid -> List LightPath
lightPaths grid =
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



-- Grid Update


updateGrid : Screen -> Mouse2 -> Grid -> Grid
updateGrid screen mouse2 grid =
    let
        ct =
            initCellT screen grid
    in
    Maybe.Extra.oneOf
        [ Mouse2.onClick (onGridTap ct)
        , Mouse2.onDrop (onGridDnd ct)
        ]
        mouse2
        |> Maybe.Extra.unwrap grid (callWith grid)


onGridDnd : CellTransform -> Float2 -> Float2 -> Grid -> Grid
onGridDnd ct dragPt dropPt grid =
    let
        mapper dragCell dropCell =
            case ( dragCell, dropCell ) of
                ( SourceWithMirror dir, Empty ) ->
                    Just ( Source, Mirror dir )

                ( SourceWithMirror dir, Source ) ->
                    Just ( Source, SourceWithMirror dir )

                ( Mirror dir, Empty ) ->
                    Just ( Empty, Mirror dir )

                ( Mirror dir, Source ) ->
                    Just ( Empty, SourceWithMirror dir )

                _ ->
                    Nothing
    in
    grid
        |> Grid.mapCellAt2 mapper (ct.fromView dragPt) (ct.fromView dropPt)
        |> Maybe.withDefault grid


onGridTap : CellTransform -> Float2 -> Grid -> Grid
onGridTap ct pt =
    rotateMirrorAt (ct.fromView pt)


rotateMirrorAt : Int2 -> Grid -> Grid
rotateMirrorAt pos grid =
    let
        ins a =
            Grid.insert pos a grid
    in
    case Grid.get pos grid of
        Just (SourceWithMirror dir) ->
            ins (SourceWithMirror (Dir.rotate 1 dir))

        Just (Mirror dir) ->
            ins (Mirror (Dir.rotate 1 dir))

        _ ->
            grid



-- Grid View


viewGrid : Time -> Mouse2 -> CellTransform -> Grid -> Shape
viewGrid time mouse2 ct grid =
    let
        lightPathsShape =
            grid
                |> lightPaths
                |> List.map (viewPath ct)
                |> group

        ( dimPos, draggedShape ) =
            getDragPosAndShape ct mouse2 grid
    in
    group
        [ gridCellShapes time ct dimPos grid |> group
        , lightPathsShape
        , draggedShape
        ]



-- DND VIEW


type alias DndView =
    { dragPos : Int2
    , dropViewPos : Float2
    , mirrorDir : Direction8
    }


toDndView : CellTransform -> Mouse2 -> Grid -> Maybe DndView
toDndView ct mouse2 grid =
    Mouse2.onDrag
        (\dragPt dropPt ->
            let
                pos =
                    ct.fromView dragPt
            in
            case getMirrorDirection pos grid of
                Just dir ->
                    Just { dragPos = pos, dropViewPos = dropPt, mirrorDir = dir }

                Nothing ->
                    Nothing
        )
        mouse2
        |> Maybe.Extra.join


getDragPosAndShape : CellTransform -> Mouse2 -> Grid -> ( Set Int2, Shape )
getDragPosAndShape ct mouse2 grid =
    case toDndView ct mouse2 grid of
        Just { dragPos, dropViewPos, mirrorDir } ->
            ( Set.singleton dragPos
            , draggedMirrorShape ct.cellSize mirrorDir dropViewPos
            )

        Nothing ->
            ( Set.empty, noShape )



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
            litDestinations grid

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


draggedMirrorShape : Float -> Direction8 -> Float2 -> Shape
draggedMirrorShape width dir viewPos =
    mirrorShape width dir
        |> scale 0.8
        |> move2 viewPos


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
