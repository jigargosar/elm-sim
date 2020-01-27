module MirrorPuzzleV3.Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
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
import Task



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
    { cellW : Float, grid : TileGrid, drag : Drag, viewExtrema : Extrema }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { cellW = 100
      , grid = initialTileGrid
      , drag = NotDragging
      , viewExtrema = toExtrema ( 600, 600 )
      }
    , Browser.Dom.getViewport |> Task.perform GotViewport
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
                ]
                [ viewTileGrid model
                ]
            , case model.drag of
                NotDragging ->
                    empty

                Dragging draggingR ->
                    [ [ [ elementShape cellW draggingR.element ]
                            |> group []
                      ]
                        |> svgCanvas (toExtrema ( cellW, cellW ))
                    ]
                        |> div
                            [ style "position" "fixed"
                            , style "top" ((fromFloat <| Tuple.second draggingR.current) ++ "px")
                            , style "left" ((fromFloat <| Tuple.first draggingR.current) ++ "px")
                            , class "pe-none"
                            ]
            ]
        ]


svgCanvas ex =
    let
        w =
            String.fromFloat ex.width

        h =
            String.fromFloat ex.height

        x =
            String.fromFloat ex.left

        y =
            String.fromFloat ex.bottom
    in
    Svg.svg
        [ SA.viewBox ([ x, y, w, h ] |> String.join " ")
        , SA.width w
        , SA.height h

        --, H.style "position" "absolute"
        --, H.style "top" "0"
        --, H.style "left" "0"
        ]


opacity =
    fromFloat >> SA.opacity


type Transform
    = Shift Float2
    | Scale Float
    | Scale2 Float Float
    | Rotate Float
    | Rotate3 Float Float Float


shift =
    Shift


scale =
    Scale


scaleY =
    Scale2 1


scaleX sx =
    Scale2 sx 1


rotate =
    Rotate


rotate3 =
    Rotate3


transformToString t =
    case t of
        Shift ( x, y ) ->
            "translate(" ++ fromFloat x ++ "," ++ fromFloat -y ++ ")"

        Scale s ->
            "scale(" ++ fromFloat s ++ ")"

        Rotate deg ->
            "rotate(" ++ fromFloat -deg ++ ")"

        Scale2 sx sy ->
            "scale(" ++ fromFloat sx ++ "," ++ fromFloat sy ++ ")"

        Rotate3 deg x y ->
            "rotate(" ++ fromFloat -deg ++ "," ++ fromFloat x ++ "," ++ fromFloat -y ++ ")"


transform : List Transform -> Svg.Attribute msg
transform =
    List.map transformToString >> String.join " " >> SA.transform


rectangle : ( Float, Float ) -> List (Svg.Attribute msg) -> Svg msg
rectangle ( width, height ) attrs =
    let
        top =
            height / 2

        left =
            -width / 2

        right =
            width / 2

        bottom =
            -height / 2
    in
    polygon [ ( left, top ), ( right, top ), ( right, bottom ), ( left, bottom ) ] attrs


polygon : List ( Float, Float ) -> List (Svg.Attribute msg) -> Svg msg
polygon points attrs =
    Svg.polygon (SA.points (List.foldl addPoint "" points) :: attrs) []


addPoint : ( Float, Float ) -> String -> String
addPoint ( x, y ) str =
    str ++ String.fromFloat x ++ "," ++ String.fromFloat -y ++ " "


square : Float -> List (Svg.Attribute msg) -> Svg msg
square w attrs =
    rectangle ( w, w ) attrs


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
        |> group [ opacity 0.6, SA.class "pe-none" ]
    , tileViewList
        |> List.map viewDebugTile
        |> group [ opacity 0.4, SA.class "pe-none" ]
    ]
        |> group [ transform [ shift gridLeftBottom ] ]


toViewPosition : Float -> Int2 -> ( Float, Float )
toViewPosition cellW position =
    position |> NT.toFloat |> NT.scale cellW


toTileView : Float -> ( Int2, Tile ) -> TileView
toTileView cellW ( position, tile ) =
    TileView position (toViewPosition cellW position) tile cellW


type alias TileView =
    { position : Int2
    , viewPosition : Float2
    , tile : Tile
    , cellW : Float
    }


viewDebugTile : { a | position : Int2, viewPosition : Float2 } -> Svg Msg
viewDebugTile { position, viewPosition } =
    words (NT.int2ToString position) [ fill "black", transform [ shift viewPosition ] ]


viewTile : TileView -> Svg Msg
viewTile { cellW, position, viewPosition, tile } =
    let
        bgBorderShape =
            [ square cellW [ fill "gray" ]
            , square cellW [ fill "white", transform [ scale 0.99 ] ]
            ]
                |> group []

        lightSourceShape =
            square cellW [ fill "orange", transform [ scale 0.9 ] ]

        elementContainerShape elementContainer =
            case elementContainer of
                Tile.LightSource ->
                    lightSourceShape

                Tile.Platform ->
                    empty

        green =
            "hsl(116, 86%, 37%)"

        goalShape : Svg msg
        goalShape =
            [ circle (cellW / 2) [ fill green, transform [ scale 0.9 ] ] ]
                |> group []

        tileShapeHelp : List (Svg.Attribute msg) -> Svg msg
        tileShapeHelp attrs =
            case tile of
                Tile.FilledContainer elementContainer element ->
                    [ bgBorderShape
                    , elementContainerShape elementContainer
                    , elementShape cellW element
                    ]
                        |> group attrs

                Tile.Wall ->
                    [ bgBorderShape
                    , square cellW [ fill "chocolate" ]
                    ]
                        |> group attrs

                Tile.EmptyContainer elementContainer ->
                    [ bgBorderShape
                    , elementContainerShape elementContainer
                    ]
                        |> group attrs

                Tile.Goal ->
                    [ bgBorderShape
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


elementShape : Float -> { a | type_ : Tile.ElementType, direction : D.Direction8 } -> Svg msg
elementShape cellW element =
    let
        mirrorShape d =
            let
                ( w, h ) =
                    ( cellW / 8, cellW / 2 )
            in
            [ ellipse w
                h
                [ fill "dodgerblue", transform [ shift ( -w, 0 ) ] ]
            ]
                |> group
                    [ transform [ scale 0.8, rotate (D.toDegrees d) ] ]

        prismShape d =
            let
                ( w, h ) =
                    ( cellW / 8, cellW / 2 )

                mirrorElipse dx =
                    ellipse w
                        h
                        [ fill "dodgerblue", transform [ shift ( dx, 0 ) ] ]
            in
            [ mirrorElipse -w
            , mirrorElipse w
            ]
                |> group [ transform [ scale 0.8, rotate (D.toDegrees d) ] ]
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
            circle (cellW / 8) (fill "red" :: attrs)

        viewEdge : ( NT.Int2, NT.Int2 ) -> Svg msg
        viewEdge points =
            let
                ( p1, p2 ) =
                    mapEach (toViewPosition cellW) points
            in
            segment p1 p2 [ outlineColor "red", thickness 3 ]
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
    | GotViewport Browser.Dom.Viewport
    | Resize Int Int


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

        GotViewport { scene } ->
            ( { model | viewExtrema = toExtrema ( scene.width, scene.height ) }, Cmd.none )

        Resize w h ->
            ( { model | viewExtrema = toExtrema (( w, h ) |> NT.toFloat) }, Cmd.none )



-- Subscriptions


clientXYDecoder =
    JD.map2 Tuple.pair
        (JD.field "clientX" JD.float)
        (JD.field "clientY" JD.float)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize Resize
        , case model.drag of
            NotDragging ->
                Sub.none

            Dragging _ ->
                [ JD.map MouseMove clientXYDecoder
                    |> Browser.Events.onMouseMove
                , JD.succeed MouseUp
                    |> Browser.Events.onMouseUp
                ]
                    |> Sub.batch
        ]
