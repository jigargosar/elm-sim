module GravitronV3.Main exposing (main)

import Basics.Extra exposing (flip)
import Browser
import Browser.Events as E
import GravitronV3.Canvas exposing (..)
import GravitronV3.Clock as Clock exposing (Clock)
import GravitronV3.Range as Range
import GravitronV3.Screen as Screen exposing (Screen)
import GravitronV3.Vec as Vec exposing (Vec, vec)
import Html exposing (Html)
import Json.Decode as D
import PointFree as FP exposing (findMapWithDefault)
import Task
import Time exposing (Posix)
import TimeTravel.Browser as TimeTravel
import Update.Pipeline exposing (..)



-- Game


type alias BodyModel a =
    { a
        | position : Vec
        , velocity : Vec
        , hp : Float
        , movement : MovementType
    }


playerConfig =
    { hp = 100
    , lives = 3
    , position = Vec.zero
    , velocity = Vec.zero
    , movement = SpringToMouse 0.2 0.5
    }


type alias PlayerModel =
    { position : Vec
    , velocity : Vec
    , hp : Float
    , movement : MovementType
    }


initialPlayer : PlayerModel
initialPlayer =
    { position = playerConfig.position
    , velocity = playerConfig.velocity
    , hp = playerConfig.hp
    , movement = playerConfig.movement
    }


type MovementType
    = GravitateToPlayer Float
    | SpringToMouse Float Float


type alias BulletModel =
    { position : Vec
    , velocity : Vec
    , hp : Float
    , movement : MovementType
    }


initBullet : MovementType -> BulletModel
initBullet movement =
    { position = Vec.vec1
    , velocity = Vec.vec1
    , hp = 1
    , movement = movement
    }


type Body
    = Bullet BulletModel
    | Player PlayerModel


type alias Game =
    { bodies : List Body }


getPlayer : List Body -> PlayerModel
getPlayer =
    findMapWithDefault playerModelFromBody initialPlayer


getPlayerPosition : List Body -> Vec
getPlayerPosition =
    getPlayer >> .position


playerModelFromBody : Body -> Maybe PlayerModel
playerModelFromBody body =
    case body of
        Player model ->
            Just model

        _ ->
            Nothing


initialGame : Game
initialGame =
    { bodies =
        [ initialPlayer |> Player
        , initBullet (GravitateToPlayer 20) |> Bullet
        ]
    }


updateGame : Env -> Game -> Game
updateGame env game =
    { game | bodies = mapBodiesWithPlayerPosition (stepBody env) game.bodies }


mapBodiesWithPlayerPosition : (Vec -> Body -> Body) -> List Body -> List Body
mapBodiesWithPlayerPosition func =
    FP.with (getPlayerPosition >> func) List.map


type alias Env =
    { mousePosition : Vec
    , screen : Screen
    }


stepBody : Env -> Vec -> Body -> Body
stepBody env playerPosition body =
    let
        stepMovementHelp =
            stepMovement env playerPosition
    in
    case body of
        Player model ->
            model |> stepMovementHelp |> Player

        Bullet model ->
            model |> stepMovementHelp |> Player


stepMovement : Env -> Vec -> BodyModel x -> BodyModel x
stepMovement { mousePosition } playerPosition model =
    let
        newVelocity =
            case model.movement of
                GravitateToPlayer g ->
                    model.velocity
                        |> Vec.add
                            (Vec.fromTo model.position playerPosition
                                |> Vec.mapMagnitude (\m -> 20 / m)
                            )

                SpringToMouse k friction ->
                    model.velocity
                        |> Vec.add (Vec.fromToScaled playerPosition mousePosition k)
                        |> Vec.scaleBy friction
    in
    { model
        | velocity = newVelocity
        , position = Vec.add model.position newVelocity
    }


viewGame : Screen -> Game -> Html msg
viewGame screen { bodies } =
    renderShapes screen
        (List.map toShape bodies)


toShape : Body -> Shape
toShape body =
    case body of
        Bullet bulletModel ->
            rect 10 10
                |> move (Vec.toTuple bulletModel.position)
                |> fill "black"

        Player playerModel ->
            rect 10 10
                |> move (Vec.toTuple playerModel.position)
                |> fill "red"


viewGame2 : Screen -> Game -> Html msg
viewGame2 screen _ =
    let
        idxToColor idx =
            if modBy 2 idx == 0 then
                "white"

            else
                "black"

        scaledShapes : List Shape
        scaledShapes =
            Range.init 1 0
                |> Range.break 100
                |> List.indexedMap
                    (\idx s ->
                        rect (screen.width * s) (screen.height * s)
                            |> stroke (idxToColor idx |> always "black")
                    )
    in
    renderShapes screen
        [ scaledShapes
            |> group
            |> fill "transparent"
        ]



-- Main


type Msg
    = GotScreen Screen
    | Tick Posix
    | MouseMove Float Float


view : Model -> Html Msg
view { screen, game } =
    viewGame screen game


type alias Model =
    { screen : Screen
    , mousePosition : Vec
    , clock : Clock
    , game : Game
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { screen = Screen.initial
      , mousePosition = Vec.zero
      , clock = Clock.initial
      , game = initialGame
      }
    , Task.perform GotScreen Screen.get
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        GotScreen screen ->
            save { model | screen = screen }

        Tick posix ->
            let
                env : Env
                env =
                    { mousePosition = model.mousePosition
                    , screen = model.screen
                    }
            in
            save
                { model
                    | clock = Clock.onAnimationFrame posix model.clock
                    , game = updateGame env model.game
                }

        MouseMove pageX pageY ->
            let
                screen =
                    model.screen
            in
            save
                { model
                    | mousePosition = vec (pageX + screen.left) (pageY + screen.top)
                }


subscriptions _ =
    Sub.batch
        [ Screen.onResize GotScreen
        , E.onAnimationFrame Tick
        , E.onMouseMove (D.map2 MouseMove (D.field "pageX" D.float) (D.field "pageY" D.float))
        ]



--noinspection ElmUnusedSymbol


timeTravelElement =
    TimeTravel.element Debug.toString Debug.toString TimeTravel.defaultConfig


main =
    --timeTravelElement
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
