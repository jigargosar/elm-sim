module GravitronV3.MainV2 exposing (main)

import Browser
import Browser.Events as E
import GravitronV3.Bullet as Bullet exposing (Bullet, BulletKind(..))
import GravitronV3.Canvas as Canvas exposing (..)
import GravitronV3.Counter as Counter exposing (Counter)
import GravitronV3.Explosion as Explosion exposing (Explosion)
import GravitronV3.Point as Pt exposing (Point)
import GravitronV3.RigidBody as RigidBody
    exposing
        ( Circular
        , CircularBody
        , RigidBody
        )
import GravitronV3.Screen as Screen exposing (Screen)
import GravitronV3.Vec as Vec exposing (Vec, vec)
import Html exposing (Html)
import Json.Decode as D
import List.Extra
import PointFree as PF
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


bounceWithinScreen : Screen -> Float -> RigidBody a -> Vec
bounceWithinScreen screen factor m =
    bounceWithinScreenHelp screen
        (m.position |> (Pt.toTuple >> Vec.fromTuple))
        factor
        m.velocity


gravitateTo : Circular target -> RigidBody model -> Vec
gravitateTo target model =
    model.velocity
        |> Vec.add
            (Pt.vecFromTo model.position target.position
                |> Vec.mapMagnitude (\mag -> 20 / mag)
            )


homingTo : Circular target -> RigidBody model -> Vec
homingTo target model =
    let
        homingVec =
            Pt.vecFromTo model.position target.position
                |> Vec.mapMagnitude (always 0.3)
    in
    model.velocity
        |> Vec.add homingVec
        |> Vec.mapMagnitude ((*) 0.98)


splitTurnInto : Int -> List Float
splitTurnInto parts =
    if parts <= 0 then
        []

    else
        let
            angleFrac =
                turns (1 / toFloat parts)
        in
        List.range 1 parts
            |> List.map ((+) -1 >> toFloat >> (*) angleFrac)



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


updatePlayer : Screen -> Player -> Player
updatePlayer screen =
    RigidBody.stepWithSeed
        [ randomWalk
        , bounceWithinScreen screen 1 >> Random.constant
        ]


playerToShape : Player -> Shape
playerToShape player =
    circle player.radius
        |> fill "green"



-- Bullet


isBulletIntersecting : BulletCtx bc -> List Bullet -> Bullet -> Bool
isBulletIntersecting ctx otherBullets bullet =
    RigidBody.doCircleOverlap bullet ctx.player
        || List.any (RigidBody.doCircleOverlap bullet) ctx.turrets
        || List.any (RigidBody.doCircleOverlap bullet)
            (List.filter (Bullet.isFakeBullet >> not) otherBullets)


type alias BulletCtx a =
    { a
        | player : Player
        , turrets : List Turret
    }


updateBullets : Screen -> BulletCtx bc -> List Bullet -> List Response
updateBullets screen ctx =
    let
        update_ : ( Bullet, List Bullet ) -> Response
        update_ ( bullet, otherBullets ) =
            if isBulletIntersecting ctx otherBullets bullet then
                Bullet.bulletToExplosion bullet |> AddExplosion

            else
                Bullet.stepBullet screen ctx.player bullet
                    |> AddBullet
    in
    List.Extra.select >> List.map update_



-- Turret Config


type TurretKind
    = GravityShooter1HP
    | GravityShooter2HP
    | TripleGravityShooter
    | GravityShooterOnDeathShoot5
    | HomingShooter
    | TimeBombShooter


turretKindToConfig : TurretKind -> TurretConfig
turretKindToConfig kind =
    let
        color =
            case kind of
                GravityShooter1HP ->
                    "red"

                GravityShooter2HP ->
                    "blue"

                TripleGravityShooter ->
                    "green"

                GravityShooterOnDeathShoot5 ->
                    "purple"

                HomingShooter ->
                    "orange"

                TimeBombShooter ->
                    "deeppink"

        maxHp =
            case kind of
                GravityShooter1HP ->
                    1

                GravityShooter2HP ->
                    2

                TripleGravityShooter ->
                    3

                GravityShooterOnDeathShoot5 ->
                    2

                HomingShooter ->
                    4

                TimeBombShooter ->
                    3

        ( bulletKind, bulletCount ) =
            case kind of
                GravityShooter1HP ->
                    ( GravityBullet, SingleBullet )

                GravityShooter2HP ->
                    ( GravityBullet, SingleBullet )

                TripleGravityShooter ->
                    ( GravityBullet, TripleBullets )

                GravityShooterOnDeathShoot5 ->
                    ( GravityBullet, SingleBullet )

                HomingShooter ->
                    ( HomingBullet, SingleBullet )

                TimeBombShooter ->
                    ( TimeBombBullet, SingleBullet )
    in
    { maxHP = maxHp
    , color = color
    , revengeOnDeath = kind == GravityShooterOnDeathShoot5
    , bulletKind = bulletKind
    , bulletCount = bulletCount
    }



-- Turret Placeholder


type alias TurretPlaceholder =
    { counter : Counter
    , position : Point
    , turret : Turret
    }


initTurretPlaceholder : Float -> Point -> TurretConfig -> TurretPlaceholder
initTurretPlaceholder delay position turretConfig =
    let
        turret =
            initTurret position turretConfig
    in
    TurretPlaceholder (Counter.initDelayedBy delay 60) position turret


turretPlaceHolderToShape : TurretPlaceholder -> Shape
turretPlaceHolderToShape { counter, turret } =
    let
        progress =
            Counter.progress counter
    in
    turretToShape turret
        |> fade progress
        |> scale progress


updateTurretPlaceHolder : TurretPlaceholder -> Response
updateTurretPlaceHolder model =
    let
        ( isDone, counter ) =
            Counter.cycleStep model.counter
    in
    if isDone then
        AddTurret model.turret

    else
        AddTurretPlaceholder { model | counter = counter }


updateTurretPlaceHolders : List TurretPlaceholder -> List Response
updateTurretPlaceHolders =
    List.map updateTurretPlaceHolder



-- Turret


type BulletCount
    = SingleBullet
    | TripleBullets
    | FiveBullets


type alias HP =
    { current : Int, max : Int }


type alias TurretConfig =
    { maxHP : Int
    , color : String
    , revengeOnDeath : Bool
    , bulletKind : BulletKind
    , bulletCount : BulletCount
    }


type alias Turret =
    { position : Point
    , velocity : Vec
    , radius : Float
    , bulletTimer : Counter
    , hp : HP
    , bulletKind : BulletKind
    , bulletCount : BulletCount
    , color : String
    , revengeOnDeath : Bool
    }


initHP : Int -> HP
initHP maxHP =
    HP maxHP maxHP


initTurret : Point -> TurretConfig -> Turret
initTurret position config =
    { position = position
    , velocity = Vec.zero
    , radius = 25
    , bulletTimer = Counter.init 60
    , hp = initHP config.maxHP
    , bulletKind = config.bulletKind
    , bulletCount = config.bulletCount
    , color = config.color
    , revengeOnDeath = config.revengeOnDeath
    }


type alias TurretCtx tc =
    { tc
        | player : Player
        , bullets : List Bullet
    }


isTurretIntersecting : TurretCtx tc -> Turret -> Bool
isTurretIntersecting ctx turret =
    ctx.bullets
        |> List.filter (Bullet.isFakeBullet >> not)
        |> List.any (RigidBody.doCircleOverlap turret)



-- || RigidBody.doCircleOverlap turret ctx.player


type alias HasHP a =
    { a | hp : HP }


isDead : HasHP a -> Bool
isDead { hp } =
    hp.current <= 0


hit : HasHP a -> HasHP a
hit hasHP =
    let
        decCurrentHP hp =
            { hp | current = max 0 (hp.current - 1) }
    in
    { hasHP | hp = decCurrentHP hasHP.hp }


hpPct : HP -> Float
hpPct hp =
    toFloat hp.current / toFloat hp.max


when =
    PF.when


fireWeaponFromTo : CircularBody a -> CircularBody b -> ( BulletKind, BulletCount ) -> Response
fireWeaponFromTo src target ( bulletKind, bulletCount ) =
    let
        addBulletWithAngle : Float -> Response
        addBulletWithAngle angle =
            Bullet.initBullet bulletKind src angle
                |> AddBullet
    in
    let
        angle =
            Pt.vecFromTo src.position target.position
                |> Vec.angle
    in
    case bulletCount of
        SingleBullet ->
            addBulletWithAngle angle

        TripleBullets ->
            let
                angleSpread =
                    turns (1 / 8)
            in
            [ angle - angleSpread, angle, angle + angleSpread ]
                |> List.map addBulletWithAngle
                |> Batch

        FiveBullets ->
            splitTurnInto 5
                |> List.map addBulletWithAngle
                |> Batch


turretDeathResponse : Player -> Turret -> Response
turretDeathResponse player turret =
    let
        addTurretExplosion =
            AddExplosion (Explosion.explosionFrom turretToShape turret)
    in
    case turret.revengeOnDeath of
        False ->
            addTurretExplosion

        True ->
            [ fireWeaponFromTo turret player ( GravityBullet, FiveBullets )
            , addTurretExplosion
            ]
                |> Batch


turretStepResponse : TurretCtx ct -> Turret -> Response
turretStepResponse ctx turret =
    let
        ( isDone, bulletTimer ) =
            Counter.cycleStep turret.bulletTimer

        addTurretResponse =
            { turret | bulletTimer = bulletTimer }
                |> when (isTurretIntersecting ctx) hit
                |> AddTurret
    in
    if isDone then
        Batch
            [ fireWeaponFromTo turret ctx.player ( turret.bulletKind, turret.bulletCount )
            , addTurretResponse
            ]

    else
        addTurretResponse


updateTurret : TurretCtx tc -> Turret -> Response
updateTurret ctx turret =
    if isDead turret then
        turretDeathResponse ctx.player turret

    else
        turretStepResponse ctx turret


updateTurrets : TurretCtx tc -> List Turret -> List Response
updateTurrets ctx =
    List.map (updateTurret ctx)


turretToShape : Turret -> Shape
turretToShape { radius, hp, color } =
    let
        fullShape =
            group
                [ circle radius
                    |> fill color
                    |> fade 0.7
                ]
    in
    group
        [ fullShape |> fade 0.9
        , fullShape |> scale (hpPct hp)
        ]



-- World


type alias World =
    { player : Player
    , turretPlaceholders : List TurretPlaceholder
    , turrets : List Turret
    , bullets : List Bullet
    , explosions : List Explosion
    }


initWorld : List TurretPlaceholder -> World
initWorld turretPlaceholders =
    { player = initialPlayer
    , turretPlaceholders = turretPlaceholders
    , turrets = []
    , bullets = []
    , explosions = []
    }


type Response
    = AddExplosion Explosion
    | AddBullet Bullet
    | AddTurret Turret
    | AddTurretPlaceholder TurretPlaceholder
    | Batch (List Response)


foldResponse : Response -> World -> World
foldResponse response world =
    case response of
        AddExplosion explosion ->
            { world | explosions = explosion :: world.explosions }

        AddBullet bullet ->
            { world | bullets = bullet :: world.bullets }

        AddTurret turret ->
            { world | turrets = turret :: world.turrets }

        Batch responses ->
            foldResponses responses world

        AddTurretPlaceholder turretPlaceHolder ->
            { world
                | turretPlaceholders =
                    turretPlaceHolder :: world.turretPlaceholders
            }


foldResponses : List Response -> World -> World
foldResponses responses world =
    List.foldl foldResponse world responses


updateWorld : Env -> World -> World
updateWorld env world =
    let
        { screen } =
            env
    in
    { world
        | player = updatePlayer screen world.player
        , turretPlaceholders = []
        , turrets = []
        , bullets = []
        , explosions = []
    }
        |> foldResponses (updateTurrets world world.turrets)
        |> foldResponses (updateBullets screen world world.bullets)
        |> foldResponses (updateTurretPlaceHolders world.turretPlaceholders)
        |> foldResponses
            (Explosion.updateExplosions world.explosions
                |> List.map AddExplosion
            )


viewWorld : World -> Shape
viewWorld world =
    group
        [ viewAllHelp turretPlaceHolderToShape world.turretPlaceholders
            |> group
        , viewAllHelp turretToShape world.turrets
            |> group
        , viewHelp playerToShape world.player
        , viewAllHelp Bullet.bulletToShape world.bullets
            |> group
        , Explosion.viewAll world.explosions
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



-- Levels


type alias LevelId =
    ( Int, Int )


type alias LevelConfig =
    List SubLevelConfig


type alias SubLevelConfig =
    List TurretKind


levels : List LevelConfig
levels =
    [ -- level 1
      [ [ GravityShooter1HP ]
      , [ GravityShooter1HP, GravityShooter1HP ]
      , [ GravityShooter1HP, GravityShooter2HP ]
      , [ GravityShooter2HP, GravityShooter2HP ]
      , List.repeat 4 GravityShooter2HP
      ]

    -- test level
    , [ [ TimeBombShooter
        ]
      , [ GravityShooter1HP, GravityShooter1HP ]
      , [ GravityShooter1HP, GravityShooter2HP ]
      , [ GravityShooter2HP, GravityShooter2HP ]
      , List.repeat 4 GravityShooter2HP
      ]
    ]


maxLevels : Int
maxLevels =
    List.length levels


nextLevelId : LevelId -> LevelId
nextLevelId ( major, minor ) =
    if minor >= 4 then
        ( major + 1 |> modBy maxLevels, 0 )

    else
        ( major, minor + 1 )


getSubLevelConfig : LevelId -> SubLevelConfig
getSubLevelConfig ( major, minor ) =
    levels
        |> List.drop major
        |> List.head
        |> Maybe.andThen (List.drop minor >> List.head)
        |> Maybe.withDefault []



-- Game


turretPositions : List Point
turretPositions =
    let
        dst =
            150
    in
    [ ( -1, -1 ), ( 1, 1 ), ( 1, -1 ), ( -1, 1 ) ]
        |> List.map (Tuple.mapBoth ((*) dst) ((*) dst) >> Pt.xy)


turretPlaceholdersForLevel : LevelId -> List TurretPlaceholder
turretPlaceholdersForLevel levelId =
    let
        subLevelConfig : SubLevelConfig
        subLevelConfig =
            getSubLevelConfig levelId

        turretCount =
            subLevelConfig |> List.length
    in
    List.map2 Tuple.pair turretPositions subLevelConfig
        |> List.indexedMap
            (\i ( position, turretKind ) ->
                initTurretPlaceholder (toFloat i / toFloat turretCount) position (turretKindToConfig turretKind)
            )


type alias Game =
    { world : World
    , levelId : LevelId
    }


initialGame : Game
initialGame =
    let
        level =
            ( 1, 0 )
    in
    { world = initWorld (turretPlaceholdersForLevel level)
    , levelId = level
    }


updateGame : Env -> Game -> Game
updateGame env game =
    let
        world =
            updateWorld env game.world

        isLevelComplete =
            List.isEmpty world.turrets && List.isEmpty world.turretPlaceholders
    in
    if isLevelComplete then
        let
            newLevel =
                nextLevelId game.levelId

            majorChanged =
                Tuple.first game.levelId /= Tuple.first newLevel

            newTurretPlaceholders =
                turretPlaceholdersForLevel newLevel

            newWorld =
                if majorChanged then
                    { world
                        | turretPlaceholders = newTurretPlaceholders
                        , bullets = []
                    }

                else
                    { world
                        | turretPlaceholders = newTurretPlaceholders
                    }
        in
        { game
            | world = newWorld
            , levelId = newLevel
        }

    else
        { game | world = world }


viewGame : Game -> Shape
viewGame game =
    group
        [ viewWorld game.world
        ]



--


type Msg
    = GotScreen Screen
    | Tick Posix
    | MouseMove Float Float


view : Model -> Html Msg
view model =
    renderShapes model.screen
        [ viewGame model.game ]


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
                    | mousePosition =
                        vec (pageX + screen.left) (pageY + screen.top)
                }


subscriptions _ =
    Sub.batch
        [ Screen.onResize GotScreen
        , E.onAnimationFrame Tick
        , D.map2 MouseMove
            (D.field "pageX" D.float)
            (D.field "pageY" D.float)
            |> E.onMouseMove
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
