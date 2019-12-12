module GravitronV3.MainV2 exposing (main)

import Browser
import Browser.Events as E
import GravitronV3.Canvas exposing (..)
import GravitronV3.Screen as Screen exposing (Screen)
import GravitronV3.Timer as Timer exposing (Timer)
import GravitronV3.Vec as Vec exposing (Vec, vec)
import GravitronV3.VelocityBehaviour as VelocityBehaviour exposing (VelocityBehaviour)
import Html exposing (Html)
import Json.Decode as D
import Task
import Time exposing (Posix)
import TimeTravel.Browser as TimeTravel
import Update.Pipeline exposing (..)



-- Player


type alias Player =
    { position : Vec
    , velocity : Vec
    , radius : Float
    , velocityBehaviour : VelocityBehaviour
    }


initialPlayer : Player
initialPlayer =
    { position = Vec.zero
    , velocity = Vec.fromRTheta 4 0
    , radius = 20
    , velocityBehaviour = VelocityBehaviour.initWanderAndBounceInScreen 1
    }


updatePlayer : Env -> Player -> Player
updatePlayer env player =
    VelocityBehaviour.updateRecord env player


viewPlayer : Player -> Shape
viewPlayer { position, radius } =
    viewFilledCircle "green" radius position



-- Bullet


type Bullet
    = Bullet
        { position : Vec
        , velocity : Vec
        , radius : Float
        , velocityBehaviour : VelocityBehaviour
        }


initBullet : Bullet
initBullet =
    Bullet
        { position = Vec.zero
        , velocity = Vec.zero
        , radius = 10
        , velocityBehaviour = VelocityBehaviour.initWanderAndBounceInScreen 0.5
        }



-- Turret


type Turret
    = Turret
        { position : Vec
        , velocity : Vec
        , radius : Float
        , bulletTimer : Timer
        }


initialTurret : Turret
initialTurret =
    Turret
        { position = Vec.vec -150 -150
        , velocity = Vec.zero
        , radius = 25
        , bulletTimer = Timer.start 0 60
        }


updateTurretHelp env turret =
    if Timer.isDone env.clock turret.bulletTimer then
        ( [], { turret | bulletTimer = Timer.restart env.clock turret.bulletTimer } )

    else
        ( [], turret )


updateTurret : Env -> Turret -> ( List Bullet, Turret )
updateTurret env (Turret turret) =
    updateTurretHelp env turret
        |> Tuple.mapSecond Turret


viewTurret : Turret -> Shape
viewTurret (Turret { position, radius }) =
    viewFilledCircle "red" radius position



-- ViewHelpers


viewFilledCircle color radius position =
    circle radius
        |> fill color
        |> move (Vec.toTuple position)



-- Game


type alias Game =
    { player : Player
    , turret : Turret
    }


initialGame : Game
initialGame =
    { player = initialPlayer
    , turret = initialTurret
    }


updateGame : Env -> Game -> Game
updateGame env ({ player } as game) =
    { game | player = updatePlayer env player }


viewGame : Game -> Shape
viewGame game =
    group
        [ viewPlayer game.player
        , viewTurret game.turret
        ]



--


type Msg
    = GotScreen Screen
    | Tick Posix
    | MouseMove Float Float


view : Model -> Html Msg
view { screen, clock, game } =
    renderShapes screen
        [ viewGame game ]


type alias Model =
    { screen : Screen
    , mousePosition : Vec
    , clock : Float
    , game : Game
    }


type alias Env =
    { mousePosition : Vec
    , screen : Screen
    , clock : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { screen = Screen.initial
      , mousePosition = Vec.zero
      , clock = 0
      , game = initialGame
      }
    , Task.perform GotScreen Screen.get
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        GotScreen screen ->
            save { model | screen = screen }

        Tick _ ->
            let
                env : Env
                env =
                    { mousePosition = model.mousePosition
                    , screen = model.screen
                    , clock = model.clock
                    }
            in
            save
                { model
                    | game = updateGame env model.game
                    , clock = model.clock + 1
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
