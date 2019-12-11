module GravitronV3.Main exposing (main)

import Browser
import Browser.Events as E
import GravitronV3.Canvas exposing (..)
import GravitronV3.Screen as Screen exposing (Screen)
import GravitronV3.Timer as Timer exposing (Timer)
import GravitronV3.Vec as Vec exposing (Vec, vec)
import Html exposing (Html)
import Json.Decode as D
import List.Extra
import PointFree as FP exposing (findWithDefault)
import Random exposing (Seed)
import Task
import Time exposing (Posix)
import TimeTravel.Browser as TimeTravel
import Update.Pipeline exposing (..)


type OnStep
    = NoOpOnStep
    | FireBulletOnStep Timer


type BodyState
    = Spawning Timer
    | Active
    | Dying Timer



-- Game


type alias Body =
    { position : Vec
    , velocity : Vec
    , radius : Float
    , state : BodyState
    , onStep : OnStep
    , hp : Float
    , movement : MovementType
    , screenCollision : ScreenCollisionType
    , type_ : BodyType
    }


type MovementType
    = GravitateToPlayer Float
    | SpringToMouse Float Float
    | Stationary
    | Wanderer Seed


type ScreenCollisionType
    = BounceWithingScreen Float
    | IgnoreScreenCollision


playerConfig : { maxHp : number, maxLives : number }
playerConfig =
    { maxHp = 100
    , maxLives = 3
    }


initialPlayer : Body
initialPlayer =
    { position = Vec.zero

    --, velocity = Vec.zero
    , velocity = Vec.fromRTheta 4 0
    , radius = 20
    , state = Spawning (Timer.start 0 60)
    , onStep = NoOpOnStep
    , hp = playerConfig.maxHp

    -- , movement = SpringToMouse 0.2 0.5
    , movement = Wanderer (Random.initialSeed 1203)

    --, screenCollision = IgnoreScreenCollision
    , screenCollision = BounceWithingScreen 1
    , type_ = Player
    }


initialGravityBullet : Body
initialGravityBullet =
    { position = vec -100 -100
    , velocity = vec 2 -1
    , radius = 10
    , state = Active
    , onStep = NoOpOnStep
    , hp = 1
    , movement = GravitateToPlayer 20
    , screenCollision = BounceWithingScreen 0.5
    , type_ = Bullet
    }


initTurret : Body
initTurret =
    { position = vec -220 -220
    , velocity = Vec.zero
    , radius = 25
    , state = Spawning (Timer.start 0 60)
    , onStep = FireBulletOnStep (Timer.start 0 (60 * 1))
    , hp = 10
    , movement = Stationary
    , screenCollision = IgnoreScreenCollision
    , type_ = Turret
    }


type BodyType
    = Bullet
    | Player
    | Turret


type alias Game =
    { bodies : List Body }


getPlayer : List Body -> Body
getPlayer =
    findWithDefault isPlayer initialPlayer


isPlayer : Body -> Bool
isPlayer body =
    case body.type_ of
        Player ->
            True

        _ ->
            False


getPlayerPosition : List Body -> Vec
getPlayerPosition =
    getPlayer >> .position


initialGame : Game
initialGame =
    { bodies =
        [ initTurret
        , initialPlayer
        , initialGravityBullet
        ]
    }


updateGame : Env -> Game -> Game
updateGame env game =
    { game
        | bodies =
            FP.with (getPlayerPosition >> stepBody env) List.map game.bodies
                |> handleCollisions
                |> FP.with (getPlayerPosition >> handleOnStep env) List.concatMap
                |> handleBodyStateTransitions env
    }


handleBodyStateTransitions : Env -> List Body -> List Body
handleBodyStateTransitions env =
    List.filterMap
        (\body ->
            case body.state of
                Dying timer ->
                    if Timer.isDone env.clock timer then
                        Nothing

                    else
                        Just body

                Spawning _ ->
                    Just body

                Active ->
                    if body.hp <= 0 then
                        Just { body | state = Dying (Timer.start env.clock 60) }

                    else
                        Just body
        )


circleCircleCollision : Body -> Body -> Bool
circleCircleCollision c1 c2 =
    Vec.lenFrom c1.position c2.position < c1.radius + c2.radius


handleCollisions : List Body -> List Body
handleCollisions =
    List.Extra.select
        >> List.map
            (\( body, others ) ->
                List.foldl handleCollisionWith body others
            )


handleCollisionWith : Body -> Body -> Body
handleCollisionWith otherBody body =
    case ( body.state, otherBody.state ) of
        ( Active, Active ) ->
            if circleCircleCollision otherBody body then
                resolveCollisionWith otherBody body

            else
                body

        _ ->
            body


resolveCollisionWith : Body -> Body -> Body
resolveCollisionWith otherBody body =
    let
        ignore =
            body

        hit =
            { body | hp = max 0 (body.hp - 1) }

        kill =
            { body | hp = 0 }
    in
    case otherBody.type_ of
        Player ->
            case body.type_ of
                Player ->
                    ignore

                Bullet ->
                    kill

                Turret ->
                    ignore

        Bullet ->
            case body.type_ of
                Player ->
                    ignore

                Bullet ->
                    kill

                Turret ->
                    hit

        Turret ->
            case body.type_ of
                Bullet ->
                    kill

                Player ->
                    ignore

                Turret ->
                    ignore


type alias Env =
    { mousePosition : Vec
    , screen : Screen
    , clock : Float
    }


stepBody : Env -> Vec -> Body -> Body
stepBody env playerPosition body =
    case body.state of
        Spawning _ ->
            body

        Active ->
            body
                |> stepMovement env playerPosition
                |> stepScreenCollision env

        Dying _ ->
            body


handleOnStep : Env -> Vec -> Body -> List Body
handleOnStep env playerPosition body =
    case body.onStep of
        NoOpOnStep ->
            [ body ]

        FireBulletOnStep timer ->
            if Timer.isDone env.clock timer then
                { body | onStep = FireBulletOnStep (Timer.restart env.clock timer) }
                    :: [ initialGravityBullet
                            |> initBulletPositionAndVelocity playerPosition body
                       ]

            else
                [ body ]


initBulletPositionAndVelocity : Vec -> Body -> Body -> Body
initBulletPositionAndVelocity toPosition fromBody body =
    let
        angle =
            Vec.fromTo fromBody.position toPosition
                |> Vec.angle
    in
    { body
        | position =
            Vec.add fromBody.position
                (Vec.fromRTheta (body.radius + fromBody.radius) angle)
        , velocity = Vec.fromRTheta 3 angle
    }


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


stepScreenCollision : Env -> Body -> Body
stepScreenCollision env body =
    case body.screenCollision of
        BounceWithingScreen bounceFactor ->
            { body
                | velocity =
                    bounceWithinScreen env.screen
                        body.position
                        bounceFactor
                        body.velocity
            }

        IgnoreScreenCollision ->
            body


stepMovement : Env -> Vec -> Body -> Body
stepMovement { mousePosition } playerPosition model =
    let
        newModel =
            case model.movement of
                GravitateToPlayer g ->
                    { model
                        | velocity =
                            model.velocity
                                |> Vec.add
                                    (Vec.fromTo model.position playerPosition
                                        |> Vec.mapMagnitude (\m -> g / m)
                                    )
                    }

                SpringToMouse k friction ->
                    { model
                        | velocity =
                            model.velocity
                                |> Vec.add (Vec.fromToScaled playerPosition mousePosition k)
                                |> Vec.scaleBy friction
                    }

                Stationary ->
                    model

                Wanderer seed ->
                    let
                        randomAngle =
                            Random.float -0.1 0.1

                        ( newAngleDiff, newSeed ) =
                            Random.step randomAngle seed
                    in
                    { model
                        | movement = Wanderer newSeed
                        , velocity = Vec.mapAngle ((+) newAngleDiff) model.velocity
                    }
    in
    { newModel | position = Vec.add newModel.position newModel.velocity }


toShape : Float -> Body -> Shape
toShape clock body =
    let
        bodyShape : Shape
        bodyShape =
            case body.type_ of
                Bullet ->
                    circle body.radius
                        |> fill "black"

                Player ->
                    circle body.radius
                        |> fill "green"

                Turret ->
                    circle body.radius
                        |> fill "tomato"

        applyBodyStateTransform : Shape -> Shape
        applyBodyStateTransform =
            case body.state of
                Spawning timer ->
                    scale (Timer.value clock timer)

                Active ->
                    identity

                Dying timer ->
                    let
                        value =
                            Timer.value clock timer
                    in
                    scale (1 + value)

        -->> fade (1 - value)
    in
    bodyShape
        |> applyBodyStateTransform
        |> move (Vec.toTuple body.position)



-- Main


type Msg
    = GotScreen Screen
    | Tick Posix
    | MouseMove Float Float


view : Model -> Html Msg
view { screen, clock, game } =
    renderShapes screen
        (List.map (toShape clock) game.bodies)


type alias Model =
    { screen : Screen
    , mousePosition : Vec
    , clock : Float
    , game : Game
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
