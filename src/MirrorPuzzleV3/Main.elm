module MirrorPuzzleV3.Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Json.Decode as JD
import MirrorPuzzleV3.Graph as Graph
import MirrorPuzzleV3.Tile as Tile exposing (Tile)
import MirrorPuzzleV3.TileGird as TileGrid exposing (TileGrid)
import Number2 as NT exposing (Float2, Int2)
import Playground.Direction8 as D
import PointFree exposing (flip, ignoreNothing, mapEach)
import String exposing (fromFloat)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events



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
    { cellW : Float, grid : TileGrid, drag : Drag }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { cellW = 100
      , grid = initialTileGrid
      , drag = NotDragging
      }
    , Cmd.none
    )


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
    div [ class "flex justify-center pv4" ]
        [ viewTileGrid model
        , case model.drag of
            NotDragging ->
                text ""

            Dragging r ->
                let
                    ( left, top ) =
                        r.current |> mapEach (fromFloat >> flip (++) "px")
                in
                div
                    [ style "width" "20px"
                    , style "height" "20px"
                    , style "top" top
                    , style "left" left
                    , class "fixed"
                    , class "lh-0"
                    ]
                    [ elementShape 100 r.element ]
        ]


group =
    Svg.g


stack =
    group


stackWithName name =
    stack [ Svg.Attributes.class name ]


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
        |> group []
    , tileViewList
        |> List.map viewDebugTile
        |> stack []
    , tileViewList
        |> List.map viewTile
        |> stackWithName "pe-all"
    ]
        |> stackWithName "pe-none"
        |> toGridSvg cellW (TileGrid.dimensions grid)


toGridSvg cellW dim =
    let
        ( w, h ) =
            dim |> NT.toFloat |> NT.scale cellW
    in
    List.singleton
        >> Svg.svg
            [ Svg.Attributes.width (fromFloat w)
            , Svg.Attributes.height (fromFloat h)
            , fill "transparent"
            ]
        >> List.singleton
        >> div []


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


rendered attrs =
    List.singleton >> stack attrs


opacity =
    fromFloat >> Svg.Attributes.opacity


type Transform
    = Shift Float2
    | Scale Float
    | Rotate Float


shift =
    Shift


scale =
    Scale


centerSquare width =
    Shift ( width / 2, width / 2 )


rotate =
    Rotate


transformToString t =
    case t of
        Shift ( x, y ) ->
            "translate(" ++ fromFloat x ++ "," ++ fromFloat y ++ ")"

        Scale s ->
            "scale(" ++ fromFloat s ++ ")"

        Rotate deg ->
            "rotate(" ++ fromFloat deg ++ ")"


transform =
    List.map transformToString >> String.join " " >> Svg.Attributes.transform


viewDebugTile : { a | position : Int2, viewPosition : Float2 } -> Svg Msg
viewDebugTile { position, viewPosition } =
    NT.int2ToString position
        |> Svg.text
        |> rendered [ opacity 0.3, transform [ shift viewPosition ] ]


square : Float -> List (Svg.Attribute msg) -> Svg msg
square w attrs =
    Svg.rect
        (Svg.Attributes.width (fromFloat w) :: Svg.Attributes.height (fromFloat w) :: attrs)
        []


segment ( x1, y1 ) ( x2, y2 ) attrs =
    Svg.line
        (Svg.Attributes.x1 (fromFloat x1)
            :: Svg.Attributes.x2 (fromFloat x2)
            :: Svg.Attributes.y1 (fromFloat y1)
            :: Svg.Attributes.y2 (fromFloat y2)
            :: attrs
        )
        []


triangle =
    square


circle r attrs =
    Svg.circle (Svg.Attributes.r (fromFloat r) :: attrs) []


ellipse w h attrs =
    Svg.ellipse (Svg.Attributes.rx (fromFloat w) :: Svg.Attributes.ry (fromFloat h) :: attrs) []


fill =
    Svg.Attributes.fill


outlineColor =
    Svg.Attributes.stroke


thickness =
    fromFloat >> Svg.Attributes.strokeWidth


empty =
    Svg.text ""


viewTile : TileView -> Svg Msg
viewTile { cellW, position, viewPosition, tile, showIndex } =
    let
        silver =
            "sliver"

        floorShape =
            square cellW [ outlineColor "black", thickness 2, transform [ centerSquare cellW ] ]

        lightSourceShape =
            square cellW [ fill "green", transform [ centerSquare cellW, scale 0.9 ] ]

        elementContainerShape elementContainer =
            case elementContainer of
                Tile.LightSource ->
                    lightSourceShape

                Tile.Platform ->
                    empty

        goalShape =
            [ circle (cellW / 2) [ fill "green", transform [ scale 0.9 ] ] ]
                |> stack []

        tileShapeHelp : List (Svg.Attribute msg) -> Svg msg
        tileShapeHelp attrs =
            case tile of
                Tile.FilledContainer elementContainer element ->
                    [ elementShape cellW element
                    , elementContainerShape elementContainer
                    , floorShape
                    ]
                        |> stack attrs

                Tile.Wall ->
                    [ square cellW [ fill silver, transform [ centerSquare cellW ] ]
                    , floorShape
                    ]
                        |> stack attrs

                Tile.EmptyContainer elementContainer ->
                    [ elementContainerShape elementContainer
                    , floorShape
                    ]
                        |> stack attrs

                Tile.Goal ->
                    [ goalShape
                    , floorShape
                    ]
                        |> stack attrs

                Tile.Hole ->
                    empty
    in
    tileShapeHelp
        [ transform [ shift viewPosition ]
        , Svg.Events.onMouseUp (CellMouseUp position)
        , Svg.Events.onClick (CellClick position)
        , Svg.Events.onMouseDown (CellMouseDown position)
        ]


elementShape cellW element =
    let
        mirrorShape d =
            ellipse (cellW / 8) (cellW / 2) [ fill "lightblue", transform [ shift ( -cellW / 2, 0 ), scale 0.8, rotate (D.toDegrees d) ] ]

        prismShape d =
            triangle cellW [ fill "lightblue", transform [ centerSquare cellW, rotate (D.toDegrees d), scale 0.8 ] ]
    in
    case element.type_ of
        Tile.Mirror ->
            mirrorShape element.direction

        Tile.Prism ->
            prismShape element.direction


viewLightPath : Float -> Graph.Graph -> Svg msg
viewLightPath cellW graph =
    let
        viewEndPoint : NT.Int2 -> Svg msg
        viewEndPoint ep =
            endPointShape [ transform [ shift (toViewPosition cellW ep) ] ]

        endPointShape attrs =
            circle (cellW / 8) (fill "black" :: opacity 0.5 :: attrs)

        viewEdge : ( NT.Int2, NT.Int2 ) -> Svg msg
        viewEdge points =
            let
                ( p1, p2 ) =
                    mapEach (toViewPosition cellW) points
            in
            segment p1 p2 [ outlineColor "black", thickness 1, opacity 0.5 ]
    in
    let
        viewEdges =
            graph
                |> Graph.edgeList
                |> List.map viewEdge
                |> stack []

        viewEndPoints =
            graph
                |> Graph.endPointList
                |> List.map viewEndPoint
                |> stack []
    in
    [ viewEndPoints
    , viewEdges
    ]
        |> stack []



-- Update


type Msg
    = NoOp
    | CellClick Int2
    | CellMouseDown Int2
    | CellMouseUp Int2
    | MouseMove Float2
    | MouseUp


type Drag
    = NotDragging
    | Dragging DraggingR


type alias DraggingR =
    { start : Int2, element : Tile.Element, current : Float2 }


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
            ( { model
                | grid =
                    (TileGrid.rotateElement position
                        |> ignoreNothing
                    )
                        model.grid
              }
            , Cmd.none
            )

        CellMouseDown position ->
            case ( model.drag, TileGrid.getMovableElement position model.grid ) of
                ( NotDragging, Just element ) ->
                    ( { model | drag = DraggingR position element ( 0, 0 ) |> Dragging }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CellMouseUp position ->
            case model.drag of
                Dragging r ->
                    ( { model
                        | drag = NotDragging
                        , grid =
                            (TileGrid.swapElements r.start position
                                |> ignoreNothing
                            )
                                model.grid
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        MouseMove pageXY ->
            case model.drag of
                NotDragging ->
                    ( model, Cmd.none )

                Dragging draggingR ->
                    ( { model | drag = Dragging { draggingR | current = pageXY } }, Cmd.none )

        MouseUp ->
            case model.drag of
                NotDragging ->
                    ( model, Cmd.none )

                Dragging _ ->
                    ( { model | drag = NotDragging }, Cmd.none )



-- Subscriptions


clientXYDecoder =
    JD.map2 Tuple.pair
        (JD.field "clientX" JD.float)
        (JD.field "clientY" JD.float)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.drag of
            NotDragging ->
                Sub.none

            Dragging _ ->
                [ JD.map MouseMove clientXYDecoder
                    |> Browser.Events.onMouseMove

                --, JD.succeed MouseUp
                --    |> Browser.Events.onMouseUp
                ]
                    |> Sub.batch
        ]
