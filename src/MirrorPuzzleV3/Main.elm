module MirrorPuzzleV3.Main exposing (main)

import Browser
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (..)
import Collage.Text as Text
import Color
import Html exposing (Html)
import MirrorPuzzleV3.Graph as Graph
import MirrorPuzzleV3.Tile as Tile exposing (Tile)
import MirrorPuzzleV3.TileGird as TileGrid exposing (TileGrid)
import Number2 as NT exposing (Float2, Int2)
import Playground.Direction8 as D
import PointFree exposing (mapEach)
import Set



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Init


type alias Model =
    { cellW : Float, grid : TileGrid }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { cellW = 100, grid = initialTileGrid }, Cmd.none )


initialTileGrid : TileGrid
initialTileGrid =
    """
              ,||,||,||,||,
            ||,__,__,__,__,||
            ||,__,__,M6,M4,||
            ||,__,S0,__,P2,||
            ||,M2,__,__,M4,||
            ||,__,__,__,__,||
            """
        |> TileGrid.decode



-- View


view : Model -> Html Msg
view model =
    viewTileGrid model


viewTileGrid : { a | cellW : Float, grid : TileGrid } -> Html Msg
viewTileGrid { cellW, grid } =
    let
        tileViewList =
            grid
                |> TileGrid.toList
                |> List.map (toTileView cellW)

        viewCellLayer =
            tileViewList
                |> List.map viewTile
                |> stack

        viewCellDebugLayer =
            tileViewList |> List.map viewDebugTile |> stack

        viewLightPathLayer =
            grid
                |> TileGrid.computeLightPaths
                |> List.concatMap (viewLightPath cellW)
                |> stack
    in
    [ viewLightPathLayer
    , viewCellDebugLayer
    , viewCellLayer
    ]
        |> stack
        --|> debug
        |> svg


toViewPosition cellW position =
    position |> NT.toFloat |> NT.scale cellW


toTileView : Float -> ( Int2, Tile ) -> TileView
toTileView cellW ( position, tile ) =
    TileView position (toViewPosition cellW position) tile True cellW


type alias TileView =
    { position : Int2
    , viewPosition : Float2
    , tile : Tile
    , showIndex : Bool
    , cellW : Float
    }


viewDebugTile { position, viewPosition } =
    NT.toStringInt2 position
        |> Text.fromString
        --|> Text.size Text.normal
        |> rendered
        |> opacity 0.3
        |> shift viewPosition


viewTile : TileView -> Collage Msg
viewTile { cellW, position, viewPosition, tile, showIndex } =
    [ Debug.toString position
        |> Text.fromString
        --|> Text.size Text.normal
        |> rendered
        |> opacity 0.3

    --|> debug
    , tileShape cellW tile
    ]
        |> stack
        |> shift viewPosition


tileShape : Float -> Tile.Tile -> Collage msg
tileShape cellW tile =
    let
        silver =
            uniform <| Color.rgb255 192 192 192

        floorShape =
            square cellW
                |> outlined (solid 0.5 silver)

        mirrorShape d =
            ellipse (cellW / 8) (cellW / 2)
                |> filled (uniform Color.lightBlue)
                |> shiftX (-cellW / 8)
                |> List.singleton
                |> stack
                |> scale 0.9
                |> List.singleton
                |> stack
                |> rotate (D.toRadians d)

        tileShapeHelp =
            case tile of
                Tile.FilledContainer _ element ->
                    case element.type_ of
                        Tile.Mirror ->
                            [ mirrorShape element.direction
                            , floorShape
                            ]
                                |> stack

                        _ ->
                            floorShape

                Tile.Wall ->
                    [ square cellW |> filled silver
                    , floorShape
                    ]
                        |> stack

                _ ->
                    floorShape
    in
    [ tileShapeHelp
    ]
        |> stack


viewLightPath : Float -> Graph.Graph -> List (Collage msg)
viewLightPath cellW graph =
    let
        viewEdges =
            Graph.getEdges graph
                |> Set.toList
                |> List.map viewEdge

        viewEndPoints =
            Graph.getEndPoints graph
                |> Set.toList
                |> List.map viewEndPoint

        viewEndPoint : NT.Int2 -> Collage msg
        viewEndPoint ep =
            endPointShape |> shift (toViewPosition cellW ep)

        endPointShape : Collage msg
        endPointShape =
            circle (cellW / 8) |> filled (uniform Color.black) |> opacity 0.5

        viewEdge : ( NT.Int2, NT.Int2 ) -> Collage msg
        viewEdge points =
            let
                ( p1, p2 ) =
                    mapEach (toViewPosition cellW) points
            in
            segment p1 p2
                |> traced (solid thin (uniform Color.black))
                |> opacity 0.5
    in
    [ viewEndPoints
    , viewEdges
    ]
        |> List.concat



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []
