module GravitronV3.MainV2 exposing (main)

import Browser
import Browser.Events as E
import GravitronV3.Canvas exposing (..)
import GravitronV3.Screen as Screen exposing (Screen)
import GravitronV3.Timer as Timer exposing (Timer)
import GravitronV3.Vec as Vec exposing (Vec, vec)
import Html exposing (Html)
import Json.Decode as D
import Random exposing (Generator, Seed)
import Random.Float
import Task
import Time exposing (Posix)
import TimeTravel.Browser as TimeTravel
import Update.Pipeline exposing (..)



-- Helpers


circleCircleCollision : { a | position : Vec, radius : Float } -> { b | position : Vec, radius : Float } -> Bool
circleCircleCollision c1 c2 =
    Vec.lenFrom c1.position c2.position < c1.radius + c2.radius


translatePosByVel : { a | position : Vec, velocity : Vec } -> { a | position : Vec, velocity : Vec }
translatePosByVel m =
    { m | position = Vec.add m.position m.velocity }


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


stepRandomWalkerVelocity : { a | velocity : Vec, seed : Seed } -> { a | velocity : Vec, seed : Seed }
stepRandomWalkerVelocity m =
    let
        ( v, s ) =
            Random.step (randomWalkerVelocity m.velocity) m.seed
    in
    { m | velocity = v, seed = s }


bounceWithinScreenHelp : Screen -> Vec -> Float -> Vec -> Vec
bounceWithinScreenHelp screen position bounceFactor velocity =
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


bounceWithinScreen : { a | screen : Screen } -> Float -> { b | position : Vec, velocity : Vec } -> { b | position : Vec, velocity : Vec }
bounceWithinScreen env factor m =
    { m | velocity = bounceWithinScreenHelp env.screen m.position factor m.velocity }


gravitateTo : { a | position : Vec } -> { b | position : Vec, velocity : Vec } -> { b | position : Vec, velocity : Vec }
gravitateTo target m =
    { m
        | velocity =
            m.velocity
                |> Vec.add
                    (Vec.fromTo m.position target.position
                        |> Vec.mapMagnitude (\mag -> 20 / mag)
                    )
    }



-- Player


type alias Player =
    { position : Vec
    , velocity : Vec
    , radius : Float
    , seed : Seed
    }


initialPlayer : Player
initialPlayer =
    { position = Vec.zero
    , velocity = Vec.fromRTheta 4 0
    , radius = 20
    , seed = Random.initialSeed 1234
    }


updatePlayer : Env -> Player -> Player
updatePlayer env =
    stepRandomWalkerVelocity
        >> bounceWithinScreen env 1
        >> translatePosByVel


viewPlayer : Player -> Shape
viewPlayer { position, radius } =
    viewFilledCircle "green" radius position



-- Bullet


type alias Bullet =
    { position : Vec
    , velocity : Vec
    , radius : Float
    }


initBullet : Bullet
initBullet =
    { position = Vec.zero
    , velocity = Vec.fromRTheta 3 0
    , radius = 10
    }


setPosVelFromTo : { a | position : Vec, radius : Float } -> { b | position : Vec } -> { c | radius : Float, position : Vec, velocity : Vec } -> { c | radius : Float, position : Vec, velocity : Vec }
setPosVelFromTo src target m =
    let
        angle =
            Vec.fromTo src.position target.position
                |> Vec.angle
    in
    { m
        | position =
            Vec.add src.position
                (Vec.fromRTheta (m.radius + src.radius) angle)
        , velocity = Vec.fromRTheta 3 angle
    }


updateBullet : Env -> Player -> Bullet -> Bullet
updateBullet env player =
    gravitateTo player
        >> bounceWithinScreen env 1
        >> translatePosByVel


updateBullets : Env -> Player -> List Bullet -> List Bullet
updateBullets env p =
    List.filterMap
        (updateBullet env p
            >> (\b ->
                    if circleCircleCollision p b then
                        Nothing

                    else
                        Just b
               )
        )


viewBullet : Bullet -> Shape
viewBullet { position, radius } =
    viewFilledCircle "black" radius position
        |> fade 0.7



-- Turret


type alias Turret =
    { position : Vec
    , velocity : Vec
    , radius : Float
    , bulletTimer : Timer
    }


initialTurret : Turret
initialTurret =
    { position = Vec.vec -150 -150
    , velocity = Vec.zero
    , radius = 25
    , bulletTimer = Timer.start 0 60
    }


updateTurret : Env -> Player -> Turret -> ( List Bullet, Turret )
updateTurret env p turret =
    if Timer.isDone env.clock turret.bulletTimer then
        ( [ initBullet |> setPosVelFromTo turret p
          ]
        , { turret | bulletTimer = Timer.restart env.clock turret.bulletTimer }
        )

    else
        ( [], turret )


viewTurret : Turret -> Shape
viewTurret { position, radius } =
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
    , bullets : List Bullet
    }


initialGame : Game
initialGame =
    { player = initialPlayer
    , turret = initialTurret
    , bullets = []
    }


updateGame : Env -> Game -> Game
updateGame env game =
    let
        ( genBullets, turret ) =
            updateTurret env game.player game.turret

        bullets =
            updateBullets env game.player game.bullets
    in
    { game
        | player = updatePlayer env game.player
        , turret = turret
        , bullets = genBullets ++ bullets
    }


viewGame : Game -> Shape
viewGame game =
    group
        [ viewPlayer game.player
        , viewTurret game.turret
        , List.map viewBullet game.bullets
            |> group
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
