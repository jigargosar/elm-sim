module MirrorPuzzleV2.PuzzleGrid exposing
    ( Model
    , fromString
    , isSolved
    , update
    , view
    )

import List.Extra
import Maybe.Extra
import MirrorPuzzleV2.Computer2 exposing (Computer2)
import MirrorPuzzleV2.Mouse2 as Mouse2 exposing (Mouse2)
import MirrorPuzzleV2.MouseEvent exposing (MouseEvent(..))
import Number2 as NT exposing (Float2, Int2)
import Playground exposing (..)
import Playground.CellTransform as CT exposing (CellTransform)
import Playground.Direction8 as Dir exposing (Direction8)
import Playground.Extra exposing (..)
import Playground.Grid as Grid
import PointFree exposing (flip, whenTrue)
import Set exposing (Set)



-- Model


type Model
    = Model Grid


fromString : String -> Model
fromString =
    decodeGrid >> fromGrid


fromGrid : Grid -> Model
fromGrid grid =
    Model grid


isSolved : Model -> Bool
isSolved (Model grid) =
    destinations grid == litDestinations grid


initCellT : Screen -> Grid -> CellTransform
initCellT screen grid =
    let
        viewD =
            NT.scale 0.8 ( screen.width, screen.height )
    in
    CT.fromViewD viewD (Grid.dimensions grid)



-- Update


update : Computer2 -> Model -> Maybe Model
update { mouse, screen } (Model grid) =
    let
        ct =
            initCellT screen grid

        updateGrid =
            case mouse.event of
                Click pt ->
                    rotateMirrorAt (ct.fromView pt)

                Drop pt1 pt2 ->
                    updateOnDrop (ct.fromView pt1) (ct.fromView pt2)

                _ ->
                    always Nothing
    in
    updateGrid grid |> Maybe.map Model



-- View


view : Computer2 -> Mouse2 -> Model -> Shape
view { time, screen } mouse2 (Model grid) =
    viewGrid time mouse2 (initCellT screen grid) grid



-- Grid Model


type alias Grid =
    Grid.Grid Cell


type Cell
    = Source
    | Destination
    | SourceWithMirror Direction8
    | Mirror Direction8
    | Wall
    | Empty



-- Grid Decoder


{-|

    S       -> source
    D       -> destination
    M<0-7>  -> mirror angle
    S<0-7>  -> source with mirror angle
    X       -> Wall
    __      -> empty cell

-}
decodeGrid : String -> Grid
decodeGrid str =
    let
        tokens : List (List String)
        tokens =
            tokenizeGridString str

        width : Int
        width =
            tokens |> List.head |> Maybe.Extra.unwrap 0 List.length

        height =
            tokens |> List.length

        insertEncodedCellAt pos cs =
            Grid.insert pos (decodeCell cs)
    in
    indexedFoldlList2d insertEncodedCellAt
        (Grid.filled width height Empty)
        tokens


tokenizeGridString : String -> List (List String)
tokenizeGridString =
    String.trim
        >> String.lines
        >> List.reverse
        >> List.map (String.split ",")


indexedFoldlList2d func =
    List.Extra.indexedFoldl
        (\y ->
            List.Extra.indexedFoldl (\x -> func ( x, y )) |> flip
        )


decodeCell : String -> Cell
decodeCell cellString =
    case String.trim cellString |> String.toList of
        'S' :: [] ->
            Source

        'D' :: [] ->
            Destination

        'X' :: [] ->
            Wall

        'M' :: [ char ] ->
            Mirror (decodeDirection char)

        'S' :: [ char ] ->
            SourceWithMirror (decodeDirection char)

        _ ->
            Empty


decodeDirection : Char -> Direction8
decodeDirection =
    String.fromChar
        >> String.toInt
        >> Maybe.withDefault 0
        >> Dir.fromInt



-- Grid Queries


destinations : Grid -> Set Int2
destinations =
    Grid.foldl (\pos cell -> whenTrue (cell == Destination) (Set.insert pos))
        Set.empty


mirrorDirectionAt : Int2 -> Grid -> Maybe Direction8
mirrorDirectionAt pos grid =
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


type alias LightPath =
    List Int2


lightPaths : Grid -> List LightPath
lightPaths grid =
    let
        reducer pos cell =
            case cell of
                SourceWithMirror dir ->
                    (::) (lightPathStartingAt pos dir grid)

                _ ->
                    identity
    in
    Grid.foldl reducer [] grid


lightPathStartingAt : Int2 -> Direction8 -> Grid -> List Int2
lightPathStartingAt pos0 dir0 grid =
    let
        accumLightPath : Direction8 -> Int2 -> List Int2 -> List Int2
        accumLightPath dir pos acc =
            let
                nextPos =
                    Dir.stepPos dir pos

                accumInDir newDir =
                    accumLightPath newDir nextPos (nextPos :: acc)
            in
            if List.member nextPos acc then
                acc

            else
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

                            Mirror newDir ->
                                accumInDir newDir

                            Wall ->
                                acc

                            Empty ->
                                accumInDir dir
    in
    accumLightPath dir0 pos0 [ pos0 ]


litDestinations : Grid -> Set Int2
litDestinations grid =
    let
        validateDestinationAt pos =
            if Grid.get pos grid == Just Destination then
                Just pos

            else
                Nothing

        getPathEndIndex =
            List.head
    in
    lightPaths grid
        |> List.filterMap
            (getPathEndIndex >> Maybe.andThen validateDestinationAt)
        |> Set.fromList



-- Grid Update


updateOnDrop : Int2 -> Int2 -> Grid.Grid Cell -> Maybe (Grid.Grid Cell)
updateOnDrop dragIdx dropIdx =
    let
        mapCellsOnDnd : Cell -> Cell -> ( Cell, Cell )
        mapCellsOnDnd drag drop =
            case ( drag, drop ) of
                ( SourceWithMirror dir, Empty ) ->
                    ( Source, Mirror dir )

                ( SourceWithMirror dir, Source ) ->
                    ( Source, SourceWithMirror dir )

                ( Mirror dir, Empty ) ->
                    ( Empty, Mirror dir )

                ( Mirror dir, Source ) ->
                    ( Empty, SourceWithMirror dir )

                _ ->
                    ( drag, drop )
    in
    Grid.update2 dragIdx dropIdx mapCellsOnDnd


rotateMirrorAt : Int2 -> Grid -> Maybe Grid
rotateMirrorAt gIdx grid =
    let
        rotateMirror cell =
            case cell of
                SourceWithMirror dir ->
                    SourceWithMirror (Dir.rotate 1 dir)

                Mirror dir ->
                    Mirror (Dir.rotate 1 dir)

                _ ->
                    cell
    in
    Grid.update gIdx rotateMirror grid



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
        (\( dragPt, dropPt ) ->
            let
                pos =
                    ct.fromView dragPt
            in
            mirrorDirectionAt pos grid
                |> Maybe.map
                    (\dir ->
                        { dragPos = pos, dropViewPos = dropPt, mirrorDir = dir }
                    )
        )
        mouse2


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
        >> (\path -> List.map2 (line red (ct.cellSize * 0.05)) path (List.drop 1 path))
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

        Wall ->
            [ rectangle darkBrown width width ]

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
        , circle lightPurple (width / 10)
        ]
        |> scale 0.9


destinationShape : Time -> Float -> Bool -> Shape
destinationShape time width isLit =
    circle blue (width / 2)
        |> whenTrue isLit (blink time)
