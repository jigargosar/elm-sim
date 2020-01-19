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
import Number2 as NT exposing (Float2, Int2)
import Playground exposing (..)
import Playground.CellTransform as CT exposing (CellTransform)
import Playground.Direction8 as Dir exposing (Direction8)
import Playground.Extra exposing (..)
import Playground.Grid as Grid
import PointFree exposing (callWith, flip, whenTrue)
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
update { mouse, screen, mouse2 } (Model grid) =
    updateGrid (initCellT screen grid) mouse2 grid
        |> Maybe.map Model



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
    | Empty



-- Grid Decoder
{-
   S       -> source
   D       -> destination
   M<0-7>   -> mirror angle
   S<0-7>   -> source with mirror
   __       -> empty cell


   -- Later
   O<0-7> -> double mirror
   X -> wall
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


mirrorDirectionAtCellPos : Int2 -> Grid -> Maybe Direction8
mirrorDirectionAtCellPos pos grid =
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


foldlSourceWithMirrors : (Int2 -> Direction8 -> a -> a) -> a -> Grid -> a
foldlSourceWithMirrors func =
    Grid.foldl
        (\pos cell ->
            case cell of
                SourceWithMirror dir ->
                    func pos dir

                _ ->
                    identity
        )


type alias LightPath =
    List Int2


lightPaths : Grid -> List LightPath
lightPaths grid =
    foldlSourceWithMirrors
        (\pos dir pathAcc ->
            lightPathStartingAt pos dir grid :: pathAcc
        )
        []
        grid


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

                        Empty ->
                            accumInDir dir
    in
    accumLightPath dir0 pos0 [ pos0 ]


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



-- Grid Update


updateGrid : CellTransform -> Mouse2 -> Grid -> Maybe Grid
updateGrid ct mouse2 grid =
    Maybe.Extra.oneOf
        [ Mouse2.onClick (onGridTap ct >> callWith grid)
        , Mouse2.onDrop (onGridDnd ct >> callWith grid)
        ]
        mouse2


onGridDnd : CellTransform -> ( Float2, Float2 ) -> Grid -> Maybe Grid
onGridDnd ct ( dragPt, dropPt ) grid =
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


onGridTap : CellTransform -> Float2 -> Grid -> Maybe Grid
onGridTap ct pt =
    rotateMirrorAt (ct.fromView pt)


rotateMirrorAt : Int2 -> Grid -> Maybe Grid
rotateMirrorAt pos grid =
    let
        ins a =
            Grid.insert pos a grid
    in
    Grid.get pos grid
        |> Maybe.map
            (\cell ->
                case cell of
                    SourceWithMirror dir ->
                        ins (SourceWithMirror (Dir.rotate 1 dir))

                    Mirror dir ->
                        ins (Mirror (Dir.rotate 1 dir))

                    _ ->
                        grid
            )



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
            mirrorDirectionAtCellPos pos grid
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
