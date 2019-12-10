module GravitronV3.Main exposing (main)

import Basics.Extra exposing (flip, uncurry)
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
import Task
import Time exposing (Posix)
import TimeTravel.Browser as TimeTravel
import Update.Pipeline exposing (..)



-- Game


type alias Body =
    { position : Vec
    , velocity : Vec
    , radius : Float
    , hp : Float
    , movement : MovementType
    , screenCollision : ScreenCollisionType
    , type_ : BodyType
    }


type MovementType
    = GravitateToPlayer Float
    | SpringToMouse Float Float
    | Stationary


type ScreenCollisionType
    = BounceWithingScreen Float
    | IgnoreScreenCollision


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
    , radius = 20
    , hp = playerConfig.hp
    , movement = playerConfig.movement
    , screenCollision = IgnoreScreenCollision
    , type_ = Player
    }


initialGravityBullet : Body
initialGravityBullet =
    { position = vec -100 -100
    , velocity = vec 2 -1
    , radius = 10
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
    , hp = 10
    , movement = Stationary
    , screenCollision = IgnoreScreenCollision
    , type_ = Turret (Timer.start 0 (60 * 1))
    }


type BodyType
    = Bullet
    | Player
    | Turret Timer


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
                |> FP.with (getPlayerPosition >> stepGenerator env) List.concatMap
                |> List.filter (.hp >> flip (>) 0)
    }


circleCircleCollision : Body -> Body -> Bool
circleCircleCollision c1 c2 =
    Vec.lenFrom c1.position c2.position < c1.radius + c2.radius


handleCollisions =
    List.Extra.select
        >> List.map (\( body, others ) -> List.foldl handleCollisionWith body others)


handleCollisionWith : Body -> Body -> Body
handleCollisionWith otherBody body =
    if circleCircleCollision otherBody body then
        resolveCollisionWith otherBody body

    else
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
    case body.type_ of
        Player ->
            case otherBody.type_ of
                Player ->
                    ignore

                Bullet ->
                    ignore

                Turret _ ->
                    ignore

        Bullet ->
            case otherBody.type_ of
                Player ->
                    kill

                Bullet ->
                    ignore

                Turret _ ->
                    kill

        Turret _ ->
            case otherBody.type_ of
                Bullet ->
                    ignore

                Player ->
                    ignore

                Turret _ ->
                    ignore


type alias Env =
    { mousePosition : Vec
    , screen : Screen
    , clock : Float
    }


stepBody : Env -> Vec -> Body -> Body
stepBody env playerPosition =
    stepMovement env playerPosition
        >> stepScreenCollision env


stepGenerator : Env -> Vec -> Body -> List Body
stepGenerator env playerPosition body =
    case body.type_ of
        Bullet ->
            [ body ]

        Player ->
            [ body ]

        Turret timer ->
            if Timer.isDone env.clock timer then
                { body | type_ = Turret (Timer.restart env.clock timer) }
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
        newVelocity =
            case model.movement of
                GravitateToPlayer g ->
                    model.velocity
                        |> Vec.add
                            (Vec.fromTo model.position playerPosition
                                |> Vec.mapMagnitude (\m -> g / m)
                            )

                SpringToMouse k friction ->
                    model.velocity
                        |> Vec.add (Vec.fromToScaled playerPosition mousePosition k)
                        |> Vec.scaleBy friction

                Stationary ->
                    Vec.zero
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
            circle body.radius
                |> move (Vec.toTuple body.position)
                |> fill "black"

        Player ->
            circle body.radius
                |> move (Vec.toTuple body.position)
                |> fill "red"

        Turret _ ->
            circle body.radius
                |> move (Vec.toTuple body.position)
                |> fill "tomato"



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

        Tick posix ->
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
                    | clock = model.clock + 1
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
