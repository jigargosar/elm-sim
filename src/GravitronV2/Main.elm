module GravitronV2.Main exposing (main)

import Array exposing (Array)
import GravitronV2.Game as G exposing (Color, Screen)
import GravitronV2.HasHealth as HasHealth
import GravitronV2.Timer as Timer exposing (Timer)
import GravitronV2.Vector2 as V exposing (Vec, vec)
import PointFree exposing (subBA)



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
    , bulletType : BulletType
    , deathType : TurretDeathType
    }


type BulletType
    = SingleBullet
    | TripleBullet
    | FiveBullets


type TurretType
    = StaticTurret
    | MovingTurret


type TurretDeathType
    = ExplodeOnDeathTurret
    | ExplodeAndReleaseFiveBulletsOnDeathTurret


type alias TurretConfig =
    { hp : Float
    , color : Color
    , bulletType : BulletType
    , turretType : TurretType
    , turretDeathType : TurretDeathType
    }


type alias StageConfig =
    List TurretConfig


stageArray : Array StageConfig
stageArray =
    let
        red1 =
            TurretConfig 1 G.red SingleBullet StaticTurret ExplodeOnDeathTurret

        blue2 =
            TurretConfig 2 G.blue SingleBullet StaticTurret ExplodeOnDeathTurret

        green3 =
            TurretConfig 3 G.green TripleBullet StaticTurret ExplodeOnDeathTurret

        blue2M5 =
            TurretConfig 2 G.blue SingleBullet StaticTurret ExplodeAndReleaseFiveBulletsOnDeathTurret
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
    , [ green3, blue2M5 ]
    , [ blue2M5, blue2M5, green3 ]
    , []
    ]
        |> Array.fromList


stageNumFromLevel : ( Int, Int ) -> Int
stageNumFromLevel ( majorLevel, minorLevel ) =
    modBy maxStages (majorLevel * 5 + minorLevel)


initTurretWithConfig : Timer -> Vec -> TurretConfig -> Turret
initTurretWithConfig triggerTimer position config =
    { position = position
    , radius = playerRadius * 1.2
    , color = config.color
    , health = HasHealth.init config.hp
    , bulletType = config.bulletType
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
                ExplodeOnDeathTurret ->
                    [ G.noShape ]

                ExplodeAndReleaseFiveBulletsOnDeathTurret ->
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
    }


defaultBullet : Bullet
defaultBullet =
    initBullet V.zero


initBullet : Vec -> Bullet
initBullet position =
    let
        speed =
            2.8

        maxSpeed =
            7
    in
    { position = position
    , velocity = vec speed speed
    , maxSpeed = maxSpeed
    , radius = 5
    , color = G.white
    , health = HasHealth.init 1
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
        gravityVec =
            V.fromPt bullet.position player.position
                |> V.mapMagnitude (\m -> 20 / m)

        newVelocity =
            [ bounceWithinScreen screen bullet.position 0.5
            , V.add gravityVec
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
                            , bulletType = t.bulletType
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


renderBullet : Bullet -> G.Shape
renderBullet bullet =
    let
        ( x, y ) =
            V.toTuple bullet.position
    in
    G.circle x y bullet.radius bullet.color



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



-- GameState


type GameState
    = Running
    | GameOver Int
    | Paused



-- Memory


type alias Memory =
    { player : Player
    , turrets : List Turret
    , bullets : List Bullet
    , bulletExplosions : List BulletExplosion
    , turretExplosions : List TurretExplosion
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
getLevelNameFromStageNum stageNum =
    let
        minorLevel =
            modBy 5 stageNum
                |> levelToString

        majorLevel =
            (maxStages // stageNum)
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
            60 * 5

        delayPerTurret =
            triggerTimerDuration / toFloat turretCountForStage

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
            8 - 1

        rTicks =
            0
    in
    { player = initPlayer
    , turrets = initTurretsForStage stage rTicks
    , bullets = []
    , bulletExplosions = []
    , turretExplosions = []
    , stage = stage
    , state = Running
    , rTicks = rTicks
    }


fireNewBullets : { from : Vec, to : Vec, offset : Float, bulletType : BulletType } -> Bullets
fireNewBullets { from, to, offset, bulletType } =
    let
        bullet =
            defaultBullet

        angleList =
            let
                angle =
                    V.fromPt from to |> V.angle
            in
            case bulletType of
                SingleBullet ->
                    List.singleton angle

                TripleBullet ->
                    [ angle - degrees 30, angle, angle + degrees 30 ]

                FiveBullets ->
                    List.range 0 4 |> List.map (toFloat >> (*) (1 / 5) >> turns >> (+) angle)

        bulletFromAngle angle =
            { bullet
                | position =
                    V.fromRTheta (offset + bullet.radius + 1) angle
                        |> V.add from
                , velocity = V.fromRTheta (V.len bullet.velocity) angle
            }
    in
    angleList |> List.map bulletFromAngle


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


handleCollision : Memory -> Memory
handleCollision model =
    model
        |> mapPlayerAndBullets (foldMap (onCircularCollisionMapBoth HasHealth.dec HasHealth.kill))
        |> mapPlayerAndTurrets (foldMap (onCircularCollisionMapBoth HasHealth.kill identity))
        |> mapTurretsAndBullets (foldMapList (onCircularCollisionMapBoth HasHealth.dec HasHealth.dec))
        |> mapBullets (foldMapSelf (onCircularCollisionMapBoth HasHealth.kill HasHealth.kill))


handleDeath : Memory -> Memory
handleDeath model =
    let
        ( newBullets, newBulletExplosions ) =
            List.partition HasHealth.isAlive model.bullets
                |> Tuple.mapSecond (List.map explosionFromBullet)

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
                            ExplodeOnDeathTurret ->
                                []

                            ExplodeAndReleaseFiveBulletsOnDeathTurret ->
                                fireNewBullets
                                    { from = t.position
                                    , to = model.player.position
                                    , offset = t.radius
                                    , bulletType = FiveBullets
                                    }
                    )
    in
    { model
        | bullets = generatedBullets ++ newBullets
        , bulletExplosions = newBulletExplosions ++ model.bulletExplosions
        , turretExplosions = newTurretExplosions ++ model.turretExplosions
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


foldMapList : (a -> b -> ( a, b )) -> List a -> List b -> ( List a, List b )
foldMapList func =
    let
        reducer a ( listA, listB ) =
            foldMap func a listB
                |> Tuple.mapFirst (\newA -> newA :: listA)
    in
    \listA listB -> List.foldl reducer ( [], listB ) listA


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


mapBullets : (a -> a) -> { b | bullets : a } -> { b | bullets : a }
mapBullets func model =
    { model | bullets = func model.bullets }


mapPlayerAndBullets func model =
    let
        ( player, bullets ) =
            func model.player model.bullets
    in
    { model | player = player, bullets = bullets }


type alias Turrets =
    List Turret


mapPlayerAndTurrets : (Player -> Turrets -> ( Player, Turrets )) -> Memory -> Memory
mapPlayerAndTurrets func model =
    let
        ( player, turrets ) =
            func model.player model.turrets
    in
    { model | player = player, turrets = turrets }


type alias Bullets =
    List Bullet


mapTurretsAndBullets : (Turrets -> Bullets -> ( Turrets, Bullets )) -> Memory -> Memory
mapTurretsAndBullets func model =
    let
        ( turrets, bullets ) =
            func model.turrets model.bullets
    in
    { model | turrets = turrets, bullets = bullets }



-- View


viewMemory : G.Computer -> Memory -> List G.Shape
viewMemory computer model =
    let
        rTicks =
            model.rTicks
    in
    renderPlayer model.player
        ++ List.map renderTurretExplosions model.turretExplosions
        ++ List.concatMap (renderTurret rTicks) model.turrets
        ++ List.map renderBullet model.bullets
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
