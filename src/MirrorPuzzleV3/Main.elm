module MirrorPuzzleV3.Main exposing (main)

import Browser
import Collage exposing (..)
import Collage.Events
import Collage.Layout exposing (..)
import Collage.Render exposing (..)
import Collage.Text as Text
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (class)
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
            ||,__,__,__,D ,||
            """
        |> TileGrid.decode



-- View


view : Model -> Html Msg
view model =
    div [ class "flex justify-center pv3" ]
        [ viewTileGrid model ]


viewTileGrid : { a | cellW : Float, grid : TileGrid } -> Html Msg
viewTileGrid { cellW, grid } =
    let
        tileViewList =
            grid
                |> TileGrid.toList
                |> List.map (toTileView cellW)
    in
    [ grid
        |> TileGrid.computeLightPaths
        |> List.map (viewLightPath cellW)
        |> stack
    , tileViewList
        |> List.map viewDebugTile
        |> stack
    , tileViewList
        |> List.map viewTile
        |> stack
        |> name "pe-all"
    ]
        |> stack
        |> name "pe-none"
        --|> debug
        |> svg


toViewPosition : Float -> Int2 -> ( Float, Float )
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


viewDebugTile : { a | position : Int2, viewPosition : Float2 } -> Collage Msg
viewDebugTile { position, viewPosition } =
    NT.int2ToString position
        |> Text.fromString
        --|> Text.size Text.normal
        |> rendered
        |> opacity 0.3
        |> shift viewPosition


viewTile : TileView -> Collage Msg
viewTile { cellW, position, viewPosition, tile, showIndex } =
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
                |> scale 0.8
                |> List.singleton
                |> stack
                |> rotate (D.toRadians d)

        prismShape d =
            triangle cellW
                |> filled (uniform Color.lightBlue)
                |> rotate (D.toRadians d)
                |> scale 0.8

        lightSourceShape =
            square cellW |> filled (uniform Color.green) |> scale 0.9

        goalShape =
            circle (cellW / 2) |> filled (uniform Color.green) |> scale 0.9

        tileShapeHelp =
            case tile of
                Tile.FilledContainer elementContainer element ->
                    [ case element.type_ of
                        Tile.Mirror ->
                            mirrorShape element.direction

                        Tile.Prism ->
                            prismShape element.direction
                    , case elementContainer of
                        Tile.LightSource ->
                            lightSourceShape

                        Tile.Floor ->
                            empty
                    , floorShape
                    ]
                        |> stack

                Tile.Wall ->
                    [ square cellW |> filled silver
                    , floorShape
                    ]
                        |> stack

                Tile.EmptyContainer elementContainer ->
                    [ case elementContainer of
                        Tile.LightSource ->
                            lightSourceShape

                        Tile.Floor ->
                            empty
                    , floorShape
                    ]
                        |> stack

                Tile.Goal ->
                    [ goalShape
                    , floorShape
                    ]
                        |> stack

                Tile.Hole ->
                    empty
    in
    tileShapeHelp
        |> shift viewPosition
        |> Collage.Events.onClick (CellClick position)


viewLightPath : Float -> Graph.Graph -> Collage msg
viewLightPath cellW graph =
    let
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
    let
        viewEdges =
            Graph.getEdges graph
                |> Set.toList
                |> List.map viewEdge
                |> stack

        viewEndPoints =
            Graph.getEndPoints graph
                |> Set.toList
                |> List.map viewEndPoint
                |> stack
    in
    [ viewEndPoints
    , viewEdges
    ]
        |> stack



-- Update


type Msg
    = NoOp
    | CellClick Int2


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        CellClick position ->
            let
                _ =
                    Debug.log "position" position
            in
            ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []
