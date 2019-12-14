module GravitronV3.MainV2 exposing (main)

import Browser
import Browser.Events as E
import GravitronV3.Canvas as Canvas exposing (..)
import GravitronV3.Point as Pt exposing (Point)
import GravitronV3.RigidBody as RigidBody exposing (CircularBody, RigidBody)
import GravitronV3.Screen as Screen exposing (Screen)
import GravitronV3.Timer as Timer exposing (Timer)
import GravitronV3.Vec as Vec exposing (Vec, vec)
import Html exposing (Html)
import Json.Decode as D
import List.Extra
import PointFree exposing (rejectWhen)
import Random exposing (Generator, Seed)
import Random.Float
import Task
import Time exposing (Posix)
import TimeTravel.Browser as TimeTravel
import Update.Pipeline exposing (..)



-- Helpers


randomWalkerVelocityGenHelp : Vec -> Generator Vec
randomWalkerVelocityGenHelp velocity =
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


randomWalk : RigidBody a -> Generator Vec
randomWalk { velocity } =
    randomWalkerVelocityGenHelp velocity


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


bounceWithinScreen : Env -> Float -> RigidBody a -> Vec
bounceWithinScreen env factor m =
    bounceWithinScreenHelp env.screen
        (m.position |> (Pt.toTuple >> Vec.fromTuple))
        factor
        m.velocity


gravitateTo : RigidBody target -> RigidBody model -> Vec
gravitateTo target model =
    model.velocity
        |> Vec.add
            (Pt.vecFromTo model.position target.position
                |> Vec.mapMagnitude (\mag -> 20 / mag)
            )



-- Response Helpers


respondWithExplosion :
    Explosion
    -> ( { b | explosions : List Explosion }, c )
    -> ( { b | explosions : List Explosion }, c )
respondWithExplosion explosion =
    Tuple.mapFirst (\res -> { res | explosions = explosion :: res.explosions })


respondWithExplosionFrom env toShapeFunc model =
    respondWithExplosion (explosionFrom env toShapeFunc model)



{-
   respondWithBullet :
       Bullet
       -> ( { b | bullets : List Bullet }, c )
       -> ( { b | bullets : List Bullet }, c )
   respondWithBullet bullet =
       Tuple.mapFirst (\res -> { res | bullets = bullet :: res.bullets })
-}


respondWithEntity : a -> ( b, List a ) -> ( b, List a )
respondWithEntity entity =
    Tuple.mapSecond ((::) entity)



-- Player


type alias Player =
    CircularBody
        { seed : Seed
        }


initialPlayer : Player
initialPlayer =
    { position = Pt.origin
    , velocity = Vec.fromRTheta 4 0
    , radius = 20
    , seed = Random.initialSeed 1234
    }


updatePlayer : Env -> Player -> Player
updatePlayer env =
    RigidBody.stepWithSeed
        [ randomWalk
        , bounceWithinScreen env 1 >> Random.constant
        ]


playerToShape : Player -> Shape
playerToShape player =
    circle player.radius
        |> fill "green"



-- Bullet


type alias Bullet =
    CircularBody {}


initBullet : Bullet
initBullet =
    { position = Pt.origin
    , velocity = Vec.fromRTheta 3 0
    , radius = 10
    }


setPosVelFromTo :
    { a | position : Point, radius : Float }
    -> { b | position : Point }
    -> { c | radius : Float, position : Point, velocity : Vec }
    -> { c | radius : Float, position : Point, velocity : Vec }
setPosVelFromTo src target m =
    let
        angle =
            Pt.vecFromTo src.position target.position
                |> Vec.angle
    in
    { m
        | position =
            Pt.moveBy (Vec.fromRTheta (m.radius + src.radius) angle)
                src.position
        , velocity = Vec.fromRTheta 3 angle
    }


isBulletIntersecting : BulletCtx bc -> List Bullet -> Bullet -> Bool
isBulletIntersecting ctx otherBullets bullet =
    RigidBody.doCircleOverlap bullet ctx.player
        || List.any (RigidBody.doCircleOverlap bullet) ctx.turrets
        || List.any (RigidBody.doCircleOverlap bullet) otherBullets


type alias BulletCtx a =
    { a
        | player : Player
        , turrets : List Turret
    }


updateBullet : Env -> BulletCtx bc -> Bullet -> Bullet
updateBullet env ctx =
    RigidBody.step
        [ gravitateTo ctx.player
        , bounceWithinScreen env 0.5
        ]


type alias BulletResponse =
    { explosions : List Explosion
    }


updateBullets : Env -> BulletCtx bc -> List Bullet -> ( BulletResponse, List Bullet )
updateBullets env ctx =
    let
        reducer :
            ( Bullet, List Bullet )
            -> ( BulletResponse, List Bullet )
            -> ( BulletResponse, List Bullet )
        reducer ( bullet, otherBullets ) =
            if isBulletIntersecting ctx otherBullets bullet then
                respondWithExplosionFrom env bulletToShape bullet

            else
                respondWithEntity (updateBullet env ctx bullet)
    in
    List.Extra.select
        >> List.foldr reducer ( BulletResponse [], [] )


bulletToShape : Bullet -> Shape
bulletToShape { radius } =
    group
        [ circle radius
            |> fill "black"
            |> fade 0.7
        ]



-- Turret


type alias Turret =
    { position : Point
    , velocity : Vec
    , radius : Float
    , bulletTimer : Timer
    }


initTurret : Float -> Point -> Turret
initTurret clock point =
    { position = point
    , velocity = Vec.zero
    , radius = 25
    , bulletTimer = Timer.start clock 60
    }


type alias TurretResponse =
    { bullets : List Bullet
    , explosions : List Explosion
    }



{-
   emptyTurretResponse : TurretResponse
   emptyTurretResponse =
       TurretResponse [] []
-}


type alias TurretCtx tc =
    { tc
        | player : Player
        , bullets : List Bullet
    }


isTurretIntersecting : TurretCtx tc -> Turret -> Bool
isTurretIntersecting ctx turret =
    List.any (RigidBody.doCircleOverlap turret) ctx.bullets



-- || RigidBody.doCircleOverlap turret ctx.player


resetBulletTimer : Env -> Turret -> Turret
resetBulletTimer env turret =
    { turret | bulletTimer = Timer.restart env.clock turret.bulletTimer }


isBulletTimerDone : Env -> Turret -> Bool
isBulletTimerDone env turret =
    Timer.isDone env.clock turret.bulletTimer



-- Update Turret V1
{-
   updateTurrets : Env -> TurretCtx tc -> List Turret -> ( TurretResponse, List Turret )
   updateTurrets env ctx =
       let
           reducer :
               Turret
               -> ( TurretResponse, List Turret )
               -> ( TurretResponse, List Turret )
           reducer turret =
               if isTurretIntersecting ctx turret then
                   respondWithExplosionFrom env turretToShape turret

               else if isBulletTimerDone env turret then
                   respondWithBullet
                       (initBullet |> setPosVelFromTo turret ctx.player)
                       >> respondWithEntity
                           (resetBulletTimer env turret)

               else
                   respondWithEntity turret
       in
       List.foldr reducer ( emptyTurretResponse, [] )
-}
-- Update Turret V2


type Response
    = AddExplosion Explosion
    | AddBullet Bullet
    | AddTurret Turret
    | Batch (List Response)


updateTurretV2 : Env -> TurretCtx tc -> Turret -> Response
updateTurretV2 env ctx turret =
    if isTurretIntersecting ctx turret then
        AddExplosion (explosionFrom env turretToShape turret)

    else if isBulletTimerDone env turret then
        Batch
            [ AddBullet (initBullet |> setPosVelFromTo turret ctx.player)
            , AddTurret (resetBulletTimer env turret)
            ]

    else
        AddTurret turret


updateTurretsV2 : Env -> TurretCtx tc -> List Turret -> Response
updateTurretsV2 env ctx =
    List.map (updateTurretV2 env ctx) >> Batch


turretToShape : Turret -> Shape
turretToShape { radius } =
    group
        [ circle radius
            |> fill "red"
            |> fade 0.7
        ]



-- Explosion


type alias Explosion =
    { position : Point
    , shape : Shape
    , timer : Timer
    }


explosionFrom : Env -> ({ a | position : Point } -> Shape) -> { a | position : Point } -> Explosion
explosionFrom env func entity =
    { position = entity.position
    , shape = func entity
    , timer = Timer.start env.clock 120
    }



{-
   initExplosion : Float -> Point -> Shape -> Explosion
   initExplosion clock position shape =
       { position = position
       , shape = shape
       , timer = Timer.start clock 120
       }
-}


updateExplosions : Env -> List Explosion -> List Explosion
updateExplosions env =
    rejectWhen (.timer >> Timer.isDone env.clock)


explosionToShape : Env -> Explosion -> Shape
explosionToShape env { position, timer, shape } =
    let
        progress =
            Timer.value env.clock timer
    in
    shape
        |> fade (1 - progress)
        |> scale (1 + progress)



-- World


type alias World =
    { player : Player
    , turrets : List Turret
    , bullets : List Bullet
    , explosions : List Explosion
    }


initWorld : List Turret -> World
initWorld turrets =
    { player = initialPlayer
    , turrets = turrets
    , bullets = []
    , explosions = []
    }


flattenResponse : Response -> List Response -> List Response
flattenResponse response acc =
    case response of
        Batch list ->
            List.foldl flattenResponse acc list

        _ ->
            response :: acc


foldResponseHelp : Response -> World -> World
foldResponseHelp response world =
    case response of
        AddExplosion explosion ->
            -- { world | explosions = explosion :: world.explosions }
            world

        AddBullet bullet ->
            -- { world | bullets = bullet :: world.bullets }
            world

        AddTurret turret ->
            { world | turrets = turret :: world.turrets }

        Batch _ ->
            Debug.todo "should never Happen"


foldResponses : List Response -> World -> World
foldResponses responses world =
    List.foldl foldResponseHelp world responses


updateWorld : Env -> World -> World
updateWorld env world =
    let
        {-
           ( turretResponse, turrets ) =
               updateTurrets env game game.turrets
        -}
        turretResponseV2 : List Response
        turretResponseV2 =
            [ updateTurretsV2 env world world.turrets ]
                |> List.foldl flattenResponse []

        ( bulletResponse, bullets ) =
            updateBullets env world world.bullets
    in
    { world
        | player = updatePlayer env world.player
        , bullets = bullets
        , turrets = []
        , explosions =
            updateExplosions env world.explosions
                ++ bulletResponse.explosions
    }
        |> foldResponses turretResponseV2


viewWorld : Env -> World -> Shape
viewWorld env game =
    group
        [ viewHelp playerToShape game.player
        , viewAllHelp turretToShape game.turrets
            |> group
        , viewAllHelp bulletToShape game.bullets
            |> group
        , viewAllHelp (explosionToShape env) game.explosions
            |> group
        ]



-- View Helpers


viewHelp toShapeFunc m =
    toShapeFunc m
        |> Canvas.move (Pt.toTuple m.position)


viewAllHelp toShapeFunc =
    List.map (viewHelp toShapeFunc)



--noinspection ElmUnusedSymbol


move =
    never



-- Game


turretPositions : List Point
turretPositions =
    let
        dst =
            150
    in
    [ ( -1, -1 ), ( 1, 1 ), ( 1, -1 ), ( -1, 1 ) ]
        |> List.map (Tuple.mapBoth ((*) dst) ((*) dst) >> Pt.xy)


turretsForLevel : Float -> Int -> List Turret
turretsForLevel clock level_ =
    let
        level =
            modBy 4 level_

        clockForIdx i =
            clock - (60 / toFloat (level + 1) * toFloat (level - i))
    in
    List.take (level + 1) turretPositions
        |> List.indexedMap (\i -> initTurret (clockForIdx i))


type alias Game =
    { world : World
    , level : Int
    }


initialGame : Game
initialGame =
    let
        level =
            3
    in
    { world = initWorld (turretsForLevel 0 level)
    , level = level
    }


updateGame : Env -> Game -> Game
updateGame env game =
    let
        world =
            updateWorld env game.world

        isLevelComplete =
            world.turrets |> List.isEmpty
    in
    if isLevelComplete then
        let
            nextLevel =
                game.level + 1
        in
        { game
            | world = { world | turrets = turretsForLevel env.clock nextLevel }
            , level = nextLevel
        }

    else
        { game | world = world }


viewGame : Env -> Game -> Shape
viewGame env game =
    group
        [ viewWorld env game.world
        ]



--


type Msg
    = GotScreen Screen
    | Tick Posix
    | MouseMove Float Float


view : Model -> Html Msg
view model =
    let
        env =
            toEnv model
    in
    renderShapes env.screen
        [ viewGame env model.game ]


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


toEnv : Model -> Env
toEnv model =
    { mousePosition = model.mousePosition
    , screen = model.screen
    , clock = model.clock
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        GotScreen screen ->
            save { model | screen = screen }

        Tick _ ->
            let
                env : Env
                env =
                    toEnv model
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
