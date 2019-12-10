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


type alias Body =
    { position : Vec
    , velocity : Vec
    , hp : Float
    , movement : MovementType
    , type_ : BodyType
    }


playerConfig =
    { hp = 100
    , lives = 3
    , position = Vec.zero
    , velocity = Vec.zero
    , movement = SpringToMouse 0.2 0.5
    }


initialPlayer : Body
initialPlayer =
    { position = playerConfig.position
    , velocity = playerConfig.velocity
    , hp = playerConfig.hp
    , movement = playerConfig.movement
    , type_ = Player
    }


type MovementType
    = GravitateToPlayer Float
    | SpringToMouse Float Float


initBullet : MovementType -> Body
initBullet movement =
    { position = Vec.vec1
    , velocity = Vec.vec1
    , hp = 1
    , movement = movement
    , type_ = Bullet
    }


type BodyType
    = Bullet
    | Player


type alias Game =
    { bodies : List Body }


getPlayer : List Body -> Body
getPlayer =
    findMapWithDefault playerModelFromBody initialPlayer


getPlayerPosition : List Body -> Vec
getPlayerPosition =
    getPlayer >> .position


playerModelFromBody : Body -> Maybe Body
playerModelFromBody body =
    case body.type_ of
        Player ->
            Just body

        _ ->
            Nothing


initialGame : Game
initialGame =
    { bodies =
        [ initialPlayer
        , initBullet (GravitateToPlayer 20)
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
stepBody env playerPosition =
    stepMovement env playerPosition


stepMovement : Env -> Vec -> Body -> Body
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
    case body.type_ of
        Bullet ->
            rect 10 10
                |> move (Vec.toTuple body.position)
                |> fill "black"

        Player ->
            rect 10 10
                |> move (Vec.toTuple body.position)
                |> fill "red"



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
