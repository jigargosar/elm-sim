module GravitronV3.MainV2 exposing (main)

import Basics.Extra exposing (flip)
import Browser
import Browser.Events as E
import GravitronV3.Canvas exposing (..)
import GravitronV3.Screen as Screen exposing (Screen)
import GravitronV3.Timer as Timer exposing (Timer)
import GravitronV3.Vec as Vec exposing (Vec, vec)
import Html exposing (Html)
import Json.Decode as D
import List.Extra
import Random exposing (Generator, Seed)
import Random.Float
import Task
import Time exposing (Posix)
import TimeTravel.Browser as TimeTravel
import Update.Pipeline exposing (..)



-- Particle


type alias Particle =
    { position : Vec
    , velocity : Vec
    , radius : Float
    , viewType : ViewType
    , behaviours : List Behaviour
    }


setVelocity : Vec -> Particle -> Particle
setVelocity velocity model =
    { model | velocity = velocity }


setVelocityIn : Particle -> Vec -> Particle
setVelocityIn =
    flip setVelocity



-- Particle Behaviour


type Behaviour
    = SpringToMouse
    | RandomWalker Seed
    | GravitateToPlayer
    | BounceWithinScreen Float
    | ApplyVelocityToPosition
    | FireGravityBulletTowardsPlayer Timer


bounceWithinScreen : Screen -> Vec -> Float -> Vec -> Vec
bounceWithinScreen screen position bounceFactor velocity =
    let
        bounceVelocityPart lo high positionPart velocityPart =
            if
                (positionPart < lo && velocityPart < 0)
                    || (positionPart > high && velocityPart > 0)
            then
                negate velocityPart

            else
                velocityPart

        ( x, y ) =
            Vec.toTuple position

        ( vx, vy ) =
            Vec.toTuple velocity

        newBouncedVelocity =
            vec (bounceVelocityPart screen.left screen.right x vx)
                (bounceVelocityPart screen.top screen.bottom y vy)
    in
    if velocity /= newBouncedVelocity then
        newBouncedVelocity |> Vec.mapMagnitude ((*) bounceFactor)

    else
        newBouncedVelocity


randomWalkerVelocity : Vec -> Generator Vec
randomWalkerVelocity velocity =
    let
        randomAngle : Generator Float
        randomAngle =
            Random.Float.standardNormal |> Random.map ((*) 0.005 >> turns)
    in
    randomAngle
        |> Random.map
            (\newAngleDiff ->
                velocity
                    |> Vec.mapAngle ((+) newAngleDiff)
                    |> Vec.mapMagnitude (max 0.01)
            )


stepParticleBehaviours : Env -> Particle -> Particle
stepParticleBehaviours env =
    let
        step : Particle -> Behaviour -> ( Particle, Behaviour )
        step model behaviour =
            case behaviour of
                RandomWalker seed ->
                    Random.step (randomWalkerVelocity model.velocity) seed
                        |> Tuple.mapBoth (setVelocityIn model) RandomWalker

                ApplyVelocityToPosition ->
                    ( { model
                        | position = Vec.add model.position model.velocity
                      }
                    , behaviour
                    )

                BounceWithinScreen bounceFactor ->
                    ( { model
                        | velocity =
                            bounceWithinScreen env.screen
                                model.position
                                bounceFactor
                                model.velocity
                      }
                    , behaviour
                    )

                _ ->
                    ( model, behaviour )
    in
    (\model -> List.Extra.mapAccumr step model model.behaviours)
        >> (\( model, behaviours ) -> { model | behaviours = behaviours })



-- Particle View


type ViewType
    = SolidCircleView String


particleToShape : Particle -> Shape
particleToShape { radius, viewType } =
    case viewType of
        SolidCircleView color ->
            circle radius |> fill color


positionParticleShape : (Particle -> Shape) -> Particle -> Shape
positionParticleShape toShapeFunc particle =
    toShapeFunc particle |> move (Vec.toTuple particle.position)


viewParticle : Particle -> Shape
viewParticle =
    positionParticleShape particleToShape



-- Player


initialPlayer : Particle
initialPlayer =
    { position = Vec.zero
    , velocity = Vec.fromRTheta 4 0
    , radius = 20
    , viewType = SolidCircleView "green"
    , behaviours =
        [ RandomWalker (Random.initialSeed 1203)
        , ApplyVelocityToPosition
        , BounceWithinScreen 1
        ]
    }



-- Turret


initialTurret : Particle
initialTurret =
    { position = Vec.vec -150 -150
    , velocity = Vec.zero
    , radius = 25
    , viewType = SolidCircleView "red"
    , behaviours =
        [ FireGravityBulletTowardsPlayer (Timer.start 0 60) ]
    }



-- Game


type alias Game =
    { player : Particle
    , turret : Particle
    }


initialGame : Game
initialGame =
    { player = initialPlayer
    , turret = initialTurret
    }


updateGame : Env -> Game -> Game
updateGame env game =
    { game | player = stepParticleBehaviours env game.player }


viewGame : Game -> Shape
viewGame game =
    group
        [ viewParticle game.player
        , viewParticle game.turret
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
