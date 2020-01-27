module MirrorPuzzleV3.Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Json.Decode as JD
import MirrorPuzzleV3.Graph as Graph
import MirrorPuzzleV3.Tile as Tile exposing (Tile)
import MirrorPuzzleV3.TileGird as TileGrid exposing (TileGrid)
import Number2 as NT exposing (Float2, Int2)
import Playground.Direction8 as D
import PointFree exposing (ignoreNothing, mapEach)
import String exposing (fromFloat)
import Svg exposing (Svg)
import Svg.Attributes as SA
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


type alias Number =
    Float


type alias Extrema =
    { width : Number
    , height : Number
    , top : Number
    , left : Number
    , right : Number
    , bottom : Number
    }


toExtrema : ( Float, Float ) -> Extrema
toExtrema ( width, height ) =
    { width = width
    , height = height
    , top = height / 2
    , left = -width / 2
    , right = width / 2
    , bottom = -height / 2
    }


view : Model -> Html Msg
view model =
    div [ class "flex justify-center pv4" ]
        [ let
            { cellW, grid } =
                model

            gridViewDimensions =
                TileGrid.dimensions grid |> (NT.toFloat >> NT.scale cellW)

            ex =
                toExtrema gridViewDimensions

            w =
                String.fromFloat ex.width

            h =
                String.fromFloat ex.height

            x =
                String.fromFloat ex.left

            y =
                String.fromFloat ex.bottom
          in
          div [ class "lh-0" ]
            [ Svg.svg
                [ SA.viewBox ([ x, y, w, h ] |> String.join " ")
                , SA.width w
                , SA.height h

                --, H.style "position" "absolute"
                --, H.style "top" "0"
                --, H.style "left" "0"
                , fill "transparent"
                ]
                [ words "FOO BAR" [] --[ transform [ shift ( w / 2, h / 2 ) ] ]
                , circle 100 [ fill "black", opacity 0.3 ]
                , viewTileGrid model
                ]
            ]
        ]


opacity =
    fromFloat >> SA.opacity


type Transform
    = Shift Float2
    | Scale Float
    | Rotate Float


shift =
    Shift


scale =
    Scale


centerSquare w =
    centerRect ( w, w )


centerRect ( w, h ) =
    Shift ( -w / 2, h / 2 )


rotate =
    Rotate


transformToString t =
    case t of
        Shift ( x, y ) ->
            "translate(" ++ fromFloat x ++ "," ++ fromFloat -y ++ ")"

        Scale s ->
            "scale(" ++ fromFloat s ++ ")"

        Rotate deg ->
            "rotate(" ++ fromFloat -deg ++ ")"


transform =
    List.map transformToString >> String.join " " >> SA.transform


square : Float -> List (Svg.Attribute msg) -> Svg msg
square w attrs =
    Svg.rect
        (SA.width (fromFloat w) :: SA.height (fromFloat w) :: attrs)
        []


squareC w attrs =
    group attrs [ square w [ transform [ centerSquare w ] ] ]


segment ( x1, y1 ) ( x2, y2 ) attrs =
    Svg.line
        (SA.x1 (fromFloat x1)
            :: SA.x2 (fromFloat x2)
            :: SA.y1 (fromFloat -y1)
            :: SA.y2 (fromFloat -y2)
            :: attrs
        )
        []


triangle r attrs =
    Svg.polygon (SA.points (toNgonPoints 0 3 r "") :: attrs) []


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


circle r attrs =
    Svg.circle (SA.r (fromFloat r) :: attrs) []


ellipse w h attrs =
    Svg.ellipse (SA.rx (fromFloat w) :: SA.ry (fromFloat h) :: attrs) []


fill =
    SA.fill


outlineColor =
    SA.stroke


thickness =
    fromFloat >> SA.strokeWidth


empty =
    Svg.text ""


words : String -> List (Svg.Attribute msg) -> Svg msg
words txt al =
    Svg.text_
        (SA.textAnchor "middle"
            :: SA.dominantBaseline "central"
            :: al
        )
        [ Svg.text txt ]


group =
    Svg.g


name =
    SA.id


viewTileGrid : { a | cellW : Float, grid : TileGrid } -> Svg Msg
viewTileGrid { cellW, grid } =
    let
        tileViewList =
            grid
                |> TileGrid.toList
                |> List.map (toTileView cellW)

        gridLeftBottom =
            let
                gd =
                    TileGrid.dimensions grid |> (NT.toFloat >> NT.scale cellW)

                cd =
                    ( cellW, cellW )
            in
            NT.sub cd gd |> NT.scale 0.5
    in
    [ tileViewList
        |> List.map viewTile
        |> group [ opacity 0.8 ]
    , TileGrid.computeLightPaths grid
        |> List.map (viewLightPath cellW)
        |> group [ opacity 0.4 ]
    , tileViewList
        |> List.map viewDebugTile
        |> group [ opacity 0.4 ]
    ]
        |> group [ name "pe-none", transform [ shift gridLeftBottom ] ]


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


viewDebugTile : { a | position : Int2, viewPosition : Float2 } -> Svg Msg
viewDebugTile { position, viewPosition } =
    words (NT.int2ToString position) [ fill "black", transform [ shift viewPosition ] ]


viewTile : TileView -> Svg Msg
viewTile { cellW, position, viewPosition, tile, showIndex } =
    let
        silver =
            "sliver"

        floorShape =
            squareC (cellW * 0.99) [ fill "black", opacity 0.1 ]

        lightSourceShape =
            squareC cellW [ fill "green", transform [ scale 0.9 ] ]

        elementContainerShape elementContainer =
            case elementContainer of
                Tile.LightSource ->
                    lightSourceShape

                Tile.Platform ->
                    empty

        goalShape =
            [ circle (cellW / 2) [ fill "green", transform [ scale 0.9 ] ] ]
                |> group []

        tileShapeHelp : List (Svg.Attribute msg) -> Svg msg
        tileShapeHelp attrs =
            case tile of
                Tile.FilledContainer elementContainer element ->
                    [ floorShape
                    , elementContainerShape elementContainer
                    , elementShape cellW element
                    ]
                        |> group attrs

                Tile.Wall ->
                    [ floorShape
                    , squareC cellW [ fill "gray" ]
                    ]
                        |> group attrs

                Tile.EmptyContainer elementContainer ->
                    [ floorShape
                    , elementContainerShape elementContainer
                    ]
                        |> group attrs

                Tile.Goal ->
                    [ floorShape
                    , goalShape
                    ]
                        |> group attrs

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
            let
                ( w, h ) =
                    ( cellW / 8, cellW / 2 )
            in
            ellipse w
                h
                [ fill "lightblue", transform [ scale 0.8, rotate (D.toDegrees d), shift ( -w / 2, 0 ) ] ]

        prismShape d =
            triangle (cellW / 2) [ fill "lightblue", transform [ rotate (D.toDegrees d), scale 0.8 ] ]
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
            circle (cellW / 8) (fill "black" :: attrs)

        viewEdge : ( NT.Int2, NT.Int2 ) -> Svg msg
        viewEdge points =
            let
                ( p1, p2 ) =
                    mapEach (toViewPosition cellW) points
            in
            segment p1 p2 [ outlineColor "black", thickness 1 ]
    in
    let
        viewEdges =
            graph
                |> Graph.edgeList
                |> List.map viewEdge
                |> group []

        viewEndPoints =
            graph
                |> Graph.endPointList
                |> List.map viewEndPoint
                |> group []
    in
    [ viewEndPoints
    , viewEdges
    ]
        |> group []



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
