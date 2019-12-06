module GravitronV2.Main exposing (main)

import Array exposing (Array)
import Basics.Extra exposing (swap)
import Color
import GravitronV2.Game as G exposing (Color, Screen, Shape)
import GravitronV2.HasHealth as HasHealth
import GravitronV2.Timer as Timer exposing (Timer)
import GravitronV2.Vector2 as V exposing (Vec, vec)
import TypedSvg
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx as InPx



-- Universal Interfaces


type alias HasPosition a =
    { a | position : Vec }



-- Geometric Interfaces


type alias HasRadius a =
    { a | radius : Float }


type alias Circular a =
    HasPosition (HasRadius a)


circleCircleCollision : Circular a -> Circular b -> Bool
circleCircleCollision c1 c2 =
    V.lenFrom c1.position c2.position <= c1.radius + c2.radius



-- Player


type alias Player =
    { position : Vec
    , velocity : Vec
    , radius : Float
    , color : G.Color
    , health : HasHealth.Health
    }


playerRadius =
    15


initPlayer : Player
initPlayer =
    { position = V.zero
    , velocity = vec 0 -10
    , radius = playerRadius
    , color = G.green
    , health = HasHealth.init 100
    }


updatePlayer : G.Mouse -> Player -> Player
updatePlayer mouse player =
    let
        springToMouseForce =
            V.fromPt player.position (V.fromRec mouse)
                -- springConstant
                |> V.multiply 0.2

        newVelocity =
            [ V.add springToMouseForce
            , V.multiply 0.5 -- friction
            ]
                |> List.foldl (<|) player.velocity
    in
    { player
        | velocity = newVelocity
        , position = V.add player.position newVelocity
    }


renderPlayer : Player -> List G.Shape
renderPlayer player =
    let
        ( x, y ) =
            V.toTuple player.position

        remainingHealthRadius =
            player.radius * HasHealth.normalized player
    in
    [ G.circle x y player.radius (G.withAlpha 0.5 player.color)
    , G.circle x y remainingHealthRadius player.color
    ]



-- Turret


type alias Turret =
    { position : Vec
    , radius : Float
    , color : G.Color
    , health : HasHealth.Health
    , triggerTimer : Timer
    , weapon : TurretWeapon
    , deathType : TurretDeathAction
    }


type TurretWeapon
    = GravitySingle
    | GravityTriple
    | GravityFive
    | HomingSingle
    | TimeBombSingle


type TurretMovement
    = StaticTurret
    | WanderingTurret


type TurretDeathAction
    = NoBulletsOnDeathTurret
    | FiveBulletsOnDeathTurret


type alias TurretConfig =
    { hp : Float
    , color : Color
    , bulletType : TurretWeapon
    , turretType : TurretMovement
    , turretDeathType : TurretDeathAction
    }


type alias StageConfig =
    List TurretConfig


stageArray : Array StageConfig
stageArray =
    let
        red1 =
            TurretConfig 1 G.red GravitySingle StaticTurret NoBulletsOnDeathTurret

        blue2 =
            TurretConfig 2 G.blue GravitySingle StaticTurret NoBulletsOnDeathTurret

        green3 =
            TurretConfig 3 G.green GravityTriple StaticTurret NoBulletsOnDeathTurret

        blue2D5Mv =
            TurretConfig 2 G.blue GravitySingle WanderingTurret FiveBulletsOnDeathTurret

        yellow5Homing =
            TurretConfig 5 G.yellow HomingSingle StaticTurret NoBulletsOnDeathTurret

        colorRose =
            G.hsl (1 / 359 * 13) 0.75 0.5

        timeBombRose =
            TurretConfig 3 colorRose TimeBombSingle StaticTurret NoBulletsOnDeathTurret
    in
    [ -- level 1
      [ red1 ]
    , [ red1, red1 ]
    , [ red1, blue2 ]
    , [ blue2, blue2 ]
    , [ blue2, blue2, blue2, blue2 ]

    -- level 2
    , [ green3 ]
    , [ red1, green3, blue2 ]
    , [ green3, blue2D5Mv ]
    , [ blue2D5Mv, blue2D5Mv, green3 ]
    , [ green3, yellow5Homing, blue2D5Mv ]

    -- level 3
    , [ timeBombRose ]
    , [ timeBombRose, yellow5Homing ]
    , []
    ]
        |> Array.fromList


stageNumFromLevel : ( Int, Int ) -> Int
stageNumFromLevel ( majorLevel, minorLevel ) =
    modBy maxStages ((majorLevel - 1) * 5 + (minorLevel - 1))


initTurretWithConfig : Timer -> Vec -> TurretConfig -> Turret
initTurretWithConfig triggerTimer position config =
    { position = position
    , radius = playerRadius * 1.2
    , color = config.color
    , health = HasHealth.init config.hp
    , weapon = config.bulletType
    , triggerTimer = triggerTimer
    , deathType = config.turretDeathType
    }


turretRestartTriggerTimerIfDone : Float -> Turret -> Turret
turretRestartTriggerTimerIfDone rTicks turret =
    { turret | triggerTimer = Timer.restartIfDone rTicks turret.triggerTimer }


renderTurret : Float -> Turret -> List G.Shape
renderTurret rTicks turret =
    let
        ( x, y ) =
            V.toTuple turret.position

        progressArcRadius =
            turret.radius + turret.radius / 4
    in
    [ G.circle x y turret.radius (G.withAlpha 0.5 turret.color)
    , -- Remaining Health Indicator
      let
        remainingHealthRadius =
            turret.radius * HasHealth.normalized turret
      in
      G.circle x y remainingHealthRadius turret.color
    , -- Trigger Arc
      let
        progress =
            Timer.value rTicks turret.triggerTimer

        xOffset =
            progressArcRadius
      in
      if progress > 0 then
        G.strokeArc ( x, y ) (turns progress) ( x + xOffset, y ) (G.withAlpha 0.5 G.white)

      else
        G.noShape
    ]
        ++ (case turret.deathType of
                NoBulletsOnDeathTurret ->
                    [ G.noShape ]

                FiveBulletsOnDeathTurret ->
                    let
                        renderBulletPlaceholderAtOffset ( ox, oy ) =
                            G.circle ox
                                oy
                                (turret.radius / 5)
                                (G.withAlpha 0.5 G.white)
                    in
                    getNEqualAngles 5
                        |> List.map
                            (V.fromRTheta progressArcRadius
                                >> V.add turret.position
                                >> V.toTuple
                                >> renderBulletPlaceholderAtOffset
                            )
           )


getNEqualAngles : Int -> List Float
getNEqualAngles n =
    List.range 0 (n - 1)
        |> List.map (toFloat >> (*) (1 / toFloat n) >> turns)



-- Bullet


type alias Bullet =
    { position : Vec
    , velocity : Vec
    , maxSpeed : Float
    , radius : Float
    , color : G.Color
    , health : HasHealth.Health
    , bulletType : BulletType
    }


type BulletType
    = GravityBullet
    | HomingBullet
    | TimeBombBullet


defaultBullet : Bullet
defaultBullet =
    let
        speed =
            2.8

        maxSpeed =
            7
    in
    { position = V.zero
    , velocity = vec speed speed
    , maxSpeed = maxSpeed
    , radius = 5
    , color = G.white
    , health = HasHealth.init 1
    , bulletType = GravityBullet
    }


bounceWithinScreen : G.Screen -> Vec -> Float -> Vec -> Vec
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
            V.toTuple position

        ( vx, vy ) =
            V.toTuple velocity

        newBouncedVelocity =
            vec (bounceVelocityPart screen.left screen.right x vx)
                (bounceVelocityPart screen.top screen.bottom y vy)
    in
    if velocity /= newBouncedVelocity then
        newBouncedVelocity |> V.mapMagnitude ((*) bounceFactor)

    else
        newBouncedVelocity


updateBullet : G.Screen -> Player -> Bullet -> Bullet
updateBullet screen player bullet =
    let
        newVelocity =
            [ bounceWithinScreen screen bullet.position 0.5
            , case bullet.bulletType of
                GravityBullet ->
                    let
                        gravityVec =
                            V.fromPt bullet.position player.position
                                |> V.mapMagnitude (\m -> 20 / m)
                    in
                    V.add gravityVec

                HomingBullet ->
                    let
                        homingVec =
                            V.fromPt bullet.position player.position
                                |> V.mapMagnitude (always 0.3)
                    in
                    V.add homingVec
                        >> V.mapMagnitude ((*) 0.98)

                TimeBombBullet ->
                    identity
            ]
                |> List.foldl (<|) bullet.velocity
    in
    { bullet
        | velocity = newVelocity
        , position = V.add bullet.position newVelocity
    }


updateBullets : Screen -> Float -> Player -> Turrets -> Bullets -> Bullets
updateBullets screen rTicks player turrets bullets =
    let
        isTurretTriggerTimerDone turret =
            Timer.isDone rTicks turret.triggerTimer

        firedBullets =
            List.foldl
                (prependWhen isTurretTriggerTimerDone
                    (\t ->
                        fireNewBullets
                            { from = t.position
                            , to = player.position
                            , offset = t.radius
                            , weapon = t.weapon
                            }
                    )
                )
                []
                turrets
                |> List.concat
    in
    firedBullets
        ++ bullets
        |> List.map (updateBullet screen player)


renderBullet : Bullet -> List Shape
renderBullet bullet =
    let
        ( x, y ) =
            V.toTuple bullet.position

        simpleBulletCircle =
            G.circle x y bullet.radius bullet.color
    in
    case bullet.bulletType of
        GravityBullet ->
            [ simpleBulletCircle ]

        HomingBullet ->
            let
                angle =
                    V.angle bullet.velocity

                extRadius =
                    bullet.radius * 2

                extVec =
                    V.fromRTheta extRadius angle

                ( x1, y1 ) =
                    V.add bullet.position extVec
                        |> V.toTuple

                ( x2, y2 ) =
                    V.add bullet.position (V.multiply -1 extVec)
                        |> V.toTuple
            in
            [ simpleBulletCircle
            , G.customShape
                (TypedSvg.line
                    [ InPx.x1 x1
                    , InPx.y1 y1
                    , InPx.x2 x2
                    , InPx.y2 y2
                    , TypedSvg.Attributes.stroke Color.white
                    ]
                    []
                )
            ]

        TimeBombBullet ->
            [ simpleBulletCircle ]



-- BulletExplosion


type alias BulletExplosion =
    { bullet : Bullet
    , maxTicks : Float
    , elapsed : Float
    }


explosionFromBullet : Bullet -> BulletExplosion
explosionFromBullet bullet =
    { bullet = bullet
    , maxTicks = 60
    , elapsed = 0
    }


stepBulletExplosionAnimation : BulletExplosion -> BulletExplosion
stepBulletExplosionAnimation model =
    { model | elapsed = model.elapsed + 1 }


isBulletExplosionAnimating : BulletExplosion -> Bool
isBulletExplosionAnimating model =
    model.elapsed < model.maxTicks


renderBulletExplosions : BulletExplosion -> G.Shape
renderBulletExplosions model =
    let
        ( x, y ) =
            V.toTuple bullet.position

        bullet =
            model.bullet

        progress =
            clamp 0 model.maxTicks model.elapsed
                / model.maxTicks

        radius =
            bullet.radius + (bullet.radius * progress)

        color =
            G.withAlpha (1 - progress) bullet.color
    in
    G.circle x y radius color



-- TurretExplosion


type alias TurretExplosion =
    { turret : Turret
    , maxTicks : Float
    , elapsed : Float
    }


explosionFromTurret : Turret -> TurretExplosion
explosionFromTurret turret =
    { turret = turret
    , maxTicks = 60
    , elapsed = 0
    }


stepTurretExplosionAnimation : TurretExplosion -> TurretExplosion
stepTurretExplosionAnimation model =
    { model | elapsed = model.elapsed + 1 }


isTurretExplosionAnimating : TurretExplosion -> Bool
isTurretExplosionAnimating model =
    model.elapsed < model.maxTicks


renderTurretExplosions : TurretExplosion -> G.Shape
renderTurretExplosions model =
    let
        ( x, y ) =
            V.toTuple turret.position

        turret =
            model.turret

        progress =
            clamp 0 model.maxTicks model.elapsed
                / model.maxTicks

        radius =
            turret.radius + (turret.radius * progress)

        color =
            G.withAlpha (1 - progress) turret.color
    in
    G.circle x y radius color



-- BlastExplosion


type alias BlastExplosion =
    { blast : Blast
    , maxTicks : Float
    , elapsed : Float
    }


explosionFromBlast : Blast -> BlastExplosion
explosionFromBlast blast =
    { blast = blast
    , maxTicks = 60
    , elapsed = 0
    }


stepBlastExplosionAnimation : BlastExplosion -> BlastExplosion
stepBlastExplosionAnimation model =
    { model | elapsed = model.elapsed + 1 }


isBlastExplosionAnimating : BlastExplosion -> Bool
isBlastExplosionAnimating model =
    model.elapsed < model.maxTicks


renderBlastExplosions : BlastExplosion -> G.Shape
renderBlastExplosions model =
    let
        ( x, y ) =
            V.toTuple blast.position

        blast =
            model.blast

        progress =
            clamp 0 model.maxTicks model.elapsed
                / model.maxTicks

        radius =
            blast.radius + (blast.radius * progress)

        color =
            G.withAlpha (1 - progress) blast.color
    in
    G.circle x y radius color



-- GameState


type GameState
    = Running
    | GameOver Int
    | Paused



-- Memory


type alias Turrets =
    List Turret


type alias Bullets =
    List Bullet


type alias Memory =
    { player : Player
    , turrets : Turrets
    , bullets : Bullets
    , blasts : Blasts
    , bulletExplosions : List BulletExplosion
    , turretExplosions : List TurretExplosion
    , blastExplosions : List BlastExplosion
    , state : GameState
    , stage : Int
    , rTicks : Float
    }


allTurretsPositions : List Vec
allTurretsPositions =
    [ vec -1 -1, vec 1 1, vec 1 -1, vec -1 1 ]
        |> List.map (V.multiply 150)


maxStages =
    stageArray |> Array.length


getLevelNameFromStageNum : Int -> String
getLevelNameFromStageNum stage =
    let
        stageNum =
            modBy maxStages stage

        minorLevel =
            modBy 5 stageNum
                |> levelToString

        majorLevel =
            (stageNum // 5)
                |> levelToString

        levelToString =
            (+) 1 >> String.fromInt
    in
    "Level " ++ majorLevel ++ "-" ++ minorLevel


getStageConfig : Int -> StageConfig
getStageConfig stageNum =
    let
        stageIdx =
            modBy maxStages stageNum
    in
    case stageArray |> Array.get stageIdx of
        Just c ->
            c

        Nothing ->
            getStageConfig stageNum


initTurretsForStage : Int -> Float -> Turrets
initTurretsForStage stageNum rTicks =
    let
        config : StageConfig
        config =
            getStageConfig stageNum

        turretCountForStage =
            List.length config

        triggerTimerDuration =
            (60 * 5)
                |> always 60

        delayPerTurret =
            (triggerTimerDuration / toFloat turretCountForStage)
                |> always 0

        triggerTimer : Int -> Timer
        triggerTimer idx =
            Timer.delayedStart rTicks
                triggerTimerDuration
                (toFloat idx * delayPerTurret)

        initTurretAtIdx : Int -> ( Vec, TurretConfig ) -> Turret
        initTurretAtIdx idx ( position, tc ) =
            initTurretWithConfig (triggerTimer idx) position tc
    in
    List.map2 Tuple.pair allTurretsPositions config
        |> List.indexedMap initTurretAtIdx


initMemory : Memory
initMemory =
    let
        stage =
            stageNumFromLevel ( 1, 1 )

        rTicks =
            0
    in
    { player = initPlayer
    , turrets = initTurretsForStage stage rTicks
    , bullets = []
    , blasts = []
    , bulletExplosions = []
    , turretExplosions = []
    , blastExplosions = []
    , stage = stage
    , state = Running
    , rTicks = rTicks
    }


fireNewBullets : { from : Vec, to : Vec, offset : Float, weapon : TurretWeapon } -> Bullets
fireNewBullets { from, to, offset, weapon } =
    let
        bullet =
            defaultBullet

        bulletFromAnge angle =
            { bullet
                | position =
                    V.fromRTheta (offset + bullet.radius + 1) angle
                        |> V.add from
                , velocity = V.fromRTheta (V.len bullet.velocity) angle
            }

        homingBullet angle =
            bulletFromAnge angle
                |> (\b -> { b | bulletType = HomingBullet })

        timeBombBullet angle =
            bulletFromAnge angle
                |> (\b -> { b | bulletType = TimeBombBullet })
    in
    let
        angle =
            V.fromPt from to |> V.angle
    in
    case weapon of
        GravitySingle ->
            [ bulletFromAnge angle ]

        GravityTriple ->
            [ angle - degrees 30, angle, angle + degrees 30 ]
                |> List.map bulletFromAnge

        GravityFive ->
            List.range 0 4
                |> List.map
                    (toFloat
                        >> (*) (1 / 5)
                        >> turns
                        >> (+) angle
                        >> bulletFromAnge
                    )

        HomingSingle ->
            [ homingBullet angle ]

        TimeBombSingle ->
            [ timeBombBullet angle ]


spacePressed =
    G.freshKeyDown " "


updateMemory : G.Computer -> Memory -> Memory
updateMemory computer model =
    case model.state of
        Running ->
            if spacePressed computer then
                { model | state = Paused }

            else
                model
                    |> updateEntities computer
                    |> handleCollision
                    |> handleDeath
                    |> incRunningTicks

        GameOver elapsed ->
            let
                maxTicks =
                    60 * 3
            in
            if elapsed > maxTicks then
                initMemory

            else
                { model | state = GameOver (elapsed + 1) }
                    |> stepAnimations

        Paused ->
            if spacePressed computer then
                { model | state = Running }

            else
                model


prependWhen : (c -> Bool) -> (c -> a) -> c -> List a -> List a
prependWhen pred t v =
    if pred v then
        (::) (t v)

    else
        identity


stepAnimations : Memory -> Memory
stepAnimations model =
    { model
        | bulletExplosions =
            List.map stepBulletExplosionAnimation model.bulletExplosions
                |> List.filter isBulletExplosionAnimating
        , turretExplosions =
            List.map stepTurretExplosionAnimation model.turretExplosions
                |> List.filter isTurretExplosionAnimating
        , blastExplosions =
            List.map stepBlastExplosionAnimation model.blastExplosions
                |> List.filter isBlastExplosionAnimating
    }


updateEntities : G.Computer -> Memory -> Memory
updateEntities computer model =
    let
        { mouse, screen } =
            computer

        { player, rTicks, turrets, bullets } =
            model
    in
    { model
        | player = updatePlayer mouse player
        , turrets = List.map (turretRestartTriggerTimerIfDone rTicks) turrets
        , bullets = updateBullets screen rTicks player turrets bullets
    }
        |> stepAnimations



-- Collision


type Entity
    = PlayerE Player
    | TurretE Turret
    | BulletE Bullet
    | BlastE Blast


type Entities
    = Entities Player (List Entity)


entitiesFromRecord :
    { a
        | player : Player
        , turrets : Turrets
        , bullets : Bullets
        , blasts : Blasts
    }
    -> Entities
entitiesFromRecord { player, turrets, bullets } =
    let
        list =
            [ PlayerE player ]
                ++ List.map TurretE turrets
                ++ List.map BulletE bullets
    in
    Entities player list


entitiesToRecord :
    Entities
    ->
        { player : Player
        , turrets : Turrets
        , bullets : Bullets
        , blasts : Blasts
        }
entitiesToRecord (Entities initialPlayer list) =
    let
        reducer e rec =
            case e of
                PlayerE player ->
                    { rec | player = player }

                TurretE turret ->
                    { rec | turrets = turret :: rec.turrets }

                BulletE bullet ->
                    { rec | bullets = bullet :: rec.bullets }

                BlastE blast ->
                    { rec | blasts = blast :: rec.blasts }
    in
    List.foldl reducer
        { player = initialPlayer
        , turrets = []
        , bullets = []
        , blasts = []
        }
        list


handleEntitiesCollision : Entities -> Entities
handleEntitiesCollision (Entities initialPlayer list) =
    Entities initialPlayer
        (foldMapSelf onEntityEntityCollision list)


onEntityEntityCollision : Entity -> Entity -> ( Entity, Entity )
onEntityEntityCollision e1 e2 =
    let
        flipCallSwap () =
            onEntityEntityCollision e2 e1 |> swap

        noOp =
            ( e1, e2 )
    in
    case ( e1, e2 ) of
        ( PlayerE p, TurretE t ) ->
            onCircularCollisionMapBoth HasHealth.kill identity p t
                |> Tuple.mapBoth PlayerE TurretE

        ( TurretE _, PlayerE _ ) ->
            flipCallSwap ()

        ( PlayerE p, BulletE b ) ->
            if b.bulletType == TimeBombBullet then
                onCircularCollisionMapBoth identity HasHealth.dec p b
                    |> Tuple.mapBoth PlayerE BulletE

            else
                onCircularCollisionMapBoth HasHealth.dec HasHealth.dec p b
                    |> Tuple.mapBoth PlayerE BulletE

        ( BulletE _, PlayerE _ ) ->
            flipCallSwap ()

        ( PlayerE p, BlastE bl ) ->
            onCircularCollisionMapBoth HasHealth.dec identity p bl
                |> Tuple.mapBoth PlayerE BlastE

        ( BlastE _, PlayerE p ) ->
            flipCallSwap ()

        ( PlayerE _, PlayerE _ ) ->
            noOp

        ( TurretE t, BulletE b ) ->
            if b.bulletType == TimeBombBullet then
                onCircularCollisionMapBoth identity HasHealth.dec t b
                    |> Tuple.mapBoth TurretE BulletE

            else
                onCircularCollisionMapBoth HasHealth.dec HasHealth.dec t b
                    |> Tuple.mapBoth TurretE BulletE

        ( BulletE _, TurretE _ ) ->
            flipCallSwap ()

        ( TurretE t, BlastE bl ) ->
            onCircularCollisionMapBoth HasHealth.dec identity t bl
                |> Tuple.mapBoth TurretE BlastE

        ( BlastE bl, TurretE t ) ->
            flipCallSwap ()

        ( TurretE _, TurretE _ ) ->
            noOp

        ( BulletE b, BlastE bl ) ->
            onCircularCollisionMapBoth HasHealth.dec identity b bl
                |> Tuple.mapBoth BulletE BlastE

        ( BlastE _, BulletE _ ) ->
            flipCallSwap ()

        ( BulletE b1, BulletE b2 ) ->
            onCircularCollisionMapBoth HasHealth.dec HasHealth.dec b1 b2
                |> Tuple.mapBoth BulletE BulletE

        ( BlastE _, BlastE _ ) ->
            noOp


handleCollision : Memory -> Memory
handleCollision model =
    let
        { player, turrets, bullets } =
            model
                |> entitiesFromRecord
                |> handleEntitiesCollision
                |> entitiesToRecord
    in
    { model | player = player, turrets = turrets, bullets = bullets }



-- DEATH


type alias Blast =
    { position : Vec
    , radius : Float
    , color : G.Color
    }


type alias Blasts =
    List Blast


blastsFromBullet : Bullet -> List Blast
blastsFromBullet bullet =
    if bullet.bulletType == TimeBombBullet then
        [ { position = bullet.position
          , radius = bullet.radius * 5
          , color = G.green
          }
        ]

    else
        []


handleDeath : Memory -> Memory
handleDeath model =
    let
        ( newBullets, deadBullets ) =
            List.partition HasHealth.isAlive model.bullets

        newBulletExplosions =
            List.map explosionFromBullet deadBullets

        newBlasts : Blasts
        newBlasts =
            List.concatMap blastsFromBullet deadBullets

        newBlastExplosions =
            List.map explosionFromBlast model.blasts

        ( newTurrets, deadTurrets ) =
            List.partition HasHealth.isAlive model.turrets

        newTurretExplosions =
            List.map explosionFromTurret deadTurrets

        generatedBullets : Bullets
        generatedBullets =
            deadTurrets
                |> List.concatMap
                    (\t ->
                        case t.deathType of
                            NoBulletsOnDeathTurret ->
                                []

                            FiveBulletsOnDeathTurret ->
                                fireNewBullets
                                    { from = t.position
                                    , to = model.player.position
                                    , offset = t.radius
                                    , weapon = GravityFive
                                    }
                    )
    in
    { model
        | bullets = generatedBullets ++ newBullets
        , blasts = newBlasts
        , bulletExplosions = newBulletExplosions ++ model.bulletExplosions
        , turretExplosions = newTurretExplosions ++ model.turretExplosions
        , blastExplosions = newBlastExplosions ++ model.blastExplosions
        , stage =
            if List.isEmpty newTurrets then
                model.stage + 1

            else
                model.stage
        , turrets =
            if List.isEmpty newTurrets then
                initTurretsForStage (model.stage + 1) model.rTicks

            else
                newTurrets
        , state =
            if model.player |> HasHealth.isDead then
                GameOver 0

            else
                model.state
    }


incRunningTicks : Memory -> Memory
incRunningTicks model =
    if model.state == Running then
        { model | rTicks = model.rTicks + 1 }

    else
        model



-- Collision Helpers


foldMap : (a -> b -> ( a, b )) -> a -> List b -> ( a, List b )
foldMap func =
    let
        reducer b ( a, bList ) =
            func a b
                |> Tuple.mapSecond (\newB -> newB :: bList)
    in
    \a bList -> List.foldl reducer ( a, [] ) bList


foldMapSelf : (a -> a -> ( a, a )) -> List a -> List a
foldMapSelf func list =
    foldMapSelfHelp func ( [], list )


foldMapSelfHelp : (a -> a -> ( a, a )) -> ( List a, List a ) -> List a
foldMapSelfHelp func ( processedList, pendingList ) =
    case pendingList of
        [] ->
            processedList

        first :: rest ->
            foldMap func first rest
                |> Tuple.mapFirst (\newFirst -> newFirst :: processedList)
                |> foldMapSelfHelp func


onCircularCollisionMapBoth :
    (Circular a -> Circular a)
    -> (Circular b -> Circular b)
    -> Circular a
    -> Circular b
    -> ( Circular a, Circular b )
onCircularCollisionMapBoth funcA funcB a b =
    if circleCircleCollision a b then
        ( funcA a, funcB b )

    else
        ( a, b )



-- View


viewMemory : G.Computer -> Memory -> List G.Shape
viewMemory computer model =
    let
        rTicks =
            model.rTicks
    in
    renderPlayer model.player
        ++ List.map renderTurretExplosions model.turretExplosions
        ++ List.map renderBlastExplosions model.blastExplosions
        ++ List.concatMap (renderTurret rTicks) model.turrets
        ++ List.concatMap renderBullet model.bullets
        ++ List.map renderBulletExplosions model.bulletExplosions
        ++ viewGameState computer.screen model.state
        ++ viewLevel computer.screen model.stage


viewLevel screen stageNum =
    let
        levelName =
            getLevelNameFromStageNum stageNum
    in
    [ G.text 0 (screen.top + 20) levelName ]


viewGameState : Screen -> GameState -> List G.Shape
viewGameState _ state =
    case state of
        Running ->
            [ G.text 0 0 "Running" ]

        GameOver _ ->
            [ G.text 0 0 "Game Over" ]

        Paused ->
            [ G.text 0 0 "Paused" ]



-- MAIN


main =
    G.game initMemory updateMemory viewMemory
