module GravitronV3.MainV2 exposing (main)

import Browser
import Browser.Events as E
import GravitronV3.Canvas exposing (..)
import GravitronV3.Screen as Screen exposing (Screen)
import GravitronV3.Vec as Vec exposing (Vec, vec)
import Html exposing (Html)
import Json.Decode as D
import List.Extra
import Random exposing (Seed)
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



-- Particle Behaviour


type Behaviour
    = SpringToMouse
    | RandomWalker Seed
    | GravitateToPlayer
    | BounceOffScreen
    | ApplyVelocityToPosition


stepParticleBehaviours : Env -> Particle -> Particle
stepParticleBehaviours env =
    let
        step : Particle -> Behaviour -> ( Particle, Behaviour )
        step model behaviour =
            case behaviour of
                RandomWalker seed ->
                    let
                        randomAngle =
                            Random.float -0.1 0.1

                        ( newAngleDiff, newSeed ) =
                            Random.step randomAngle seed
                    in
                    ( { model
                        | velocity =
                            model.velocity
                                |> Vec.mapAngle ((+) newAngleDiff)
                                |> Vec.mapMagnitude (max 1)
                      }
                    , RandomWalker newSeed
                    )

                ApplyVelocityToPosition ->
                    ( { model
                        | position = Vec.add model.position model.velocity
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
        , BounceOffScreen
        ]
    }



-- Game


type alias Game =
    { player : Particle }


initialGame : Game
initialGame =
    { player = initialPlayer
    }


updateGame : Env -> Game -> Game
updateGame env game =
    { game | player = stepParticleBehaviours env game.player }


viewGame : Game -> Shape
viewGame game =
    group
        [ viewParticle game.player
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
