module GravitronV2.Main exposing (main)

import Array exposing (Array)
import Basics.Extra exposing (flip, swap)
import GravitronV2.Bullet as Bullet exposing (Bullet)
import GravitronV2.Counter as Counter exposing (Counter)
import GravitronV2.Game as G exposing (Color, Screen, Shape)
import GravitronV2.HasHealth as HasHealth
import GravitronV2.Particle as Particle exposing (HasPosition, Particle)
import GravitronV2.Timer as Timer exposing (Timer)
import GravitronV2.Vec as V exposing (Vec, vec)
import List.Extra



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


stepPlayer : G.Mouse -> Player -> Player
stepPlayer mouse player =
    player
        |> Particle.mapVelocity
            (V.add (V.fromToScaled player.position mouse.position 0.2)
                >> V.scaleBy 0.5
            )
        |> Particle.step


renderPlayer : Player -> G.Shape
renderPlayer player =
    let
        ( x, y ) =
            V.toTuple player.position

        remainingHealthRadius =
            player.radius * HasHealth.normalized player
    in
    [ G.circle player.radius (G.withAlpha 0.5 player.color)
    , G.circle remainingHealthRadius player.color
    ]
        |> G.group
        |> G.move x y



-- Turret


type alias Turret =
    Particle
        { color : G.Color
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
    , velocity = V.zero
    , radius = playerRadius * 1.2
    , color = config.color
    , health = HasHealth.init config.hp
    , weapon = config.bulletType
    , triggerTimer = triggerTimer
    , deathType = config.turretDeathType
    }


stepTurret : Float -> HasPosition a -> Turret -> ( Bullets, Turret )
stepTurret rTicks target turret =
    ( if Timer.isDone rTicks turret.triggerTimer then
        turretGenerateBulletForWeapon rTicks target turret.weapon turret

      else
        []
    , { turret | triggerTimer = Timer.restartIfDone rTicks turret.triggerTimer }
    )


turretGenerateBulletForWeapon : Float -> HasPosition a -> TurretWeapon -> Turret -> Bullets
turretGenerateBulletForWeapon rTicks target weapon turret =
    let
        from =
            turret.position

        to =
            target.position

        configFromAngle angle =
            { position = turret.position
            , offset = turret.radius
            , angle = angle
            }
    in
    let
        angle =
            V.fromPt from to |> V.angle
    in
    case weapon of
        GravitySingle ->
            [ Bullet.initGravity (configFromAngle angle) ]

        GravityTriple ->
            [ angle - degrees 30, angle, angle + degrees 30 ]
                |> List.map (configFromAngle >> Bullet.initGravity)

        GravityFive ->
            List.range 0 4
                |> List.map
                    (toFloat
                        >> (*) (1 / 5)
                        >> turns
                        >> (+) angle
                        >> configFromAngle
                        >> Bullet.initGravity
                    )

        HomingSingle ->
            [ Bullet.initHoming (configFromAngle angle) ]

        TimeBombSingle ->
            [ Bullet.initTimeBomb rTicks (configFromAngle angle) ]


renderTurret : Float -> Turret -> G.Shape
renderTurret rTicks turret =
    let
        ( x, y ) =
            V.toTuple turret.position
    in
    renderTurretHelp rTicks turret |> G.group |> G.move x y


renderTurretHelp : Float -> Turret -> List Shape
renderTurretHelp rTicks turret =
    let
        ( x, y ) =
            ( 0, 0 )

        position =
            V.zero

        progressArcRadius =
            turret.radius + turret.radius / 4
    in
    [ G.circleAt x y turret.radius (G.withAlpha 0.5 turret.color)
    , -- Remaining Health Indicator
      let
        remainingHealthRadius =
            turret.radius * HasHealth.normalized turret
      in
      G.circleAt x y remainingHealthRadius turret.color
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
                            G.circleAt ox
                                oy
                                (turret.radius / 5)
                                (G.withAlpha 0.5 G.white)
                    in
                    getNEqualAngles 5
                        |> List.map
                            (V.fromRTheta progressArcRadius
                                >> V.add position
                                >> V.toTuple
                                >> renderBulletPlaceholderAtOffset
                            )
           )


getNEqualAngles : Int -> List Float
getNEqualAngles n =
    List.range 0 (n - 1)
        |> List.map (toFloat >> (*) (1 / toFloat n) >> turns)



-- DA


type alias DeathAnimation =
    { timer : Timer
    , kind : DeathAnimationKind
    }


renderDeathAnimations : HasDeathAnimations a -> List Shape
renderDeathAnimations model =
    List.map (renderDeathAnimation model.deathAnimationsClock) model.deathAnimations


renderDeathAnimation : Float -> DeathAnimation -> G.Shape
renderDeathAnimation clock anim =
    let
        progress =
            Timer.value clock anim.timer

        timeOfDeath =
            Timer.getStart anim.timer

        maxOpacity =
            0.8

        shapeOfAnim =
            case anim.kind of
                BulletDeathAnim bullet ->
                    Bullet.renderBullet timeOfDeath bullet

                TurretDeathAnim turret ->
                    renderTurret timeOfDeath turret

                BlastDeathAnim blast ->
                    let
                        ( x, y ) =
                            V.toTuple blast.position
                    in
                    G.circle blast.radius blast.color
                        |> G.move x y

                PlayerDeathAnim player ->
                    renderPlayer player
    in
    shapeOfAnim
        |> G.scale (1 + progress)
        |> G.fade (maxOpacity - (progress * maxOpacity))



-- Entities


type Entity
    = PlayerE Player
    | TurretE Turret
    | BulletE Bullet
    | BlastE Blast


type Entities
    = Entities Player (List Entity)


entitiesFromHasGameObjects : HasGameObjects a -> Entities
entitiesFromHasGameObjects { player, turrets, bullets, blasts } =
    let
        list =
            [ PlayerE player ]
                ++ List.map TurretE turrets
                ++ List.map BulletE bullets
                ++ List.map BlastE blasts
    in
    Entities player list


replaceGameObjectsFromEntities : Entities -> HasGameObjects a -> HasGameObjects a
replaceGameObjectsFromEntities (Entities initialPlayer list) model =
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

        newGameObjects =
            List.foldl reducer
                { player = initialPlayer
                , turrets = []
                , bullets = []
                , blasts = []
                }
                list
    in
    replaceGameObjectsFrom newGameObjects model


replaceGameObjectsFrom : HasGameObjects a -> HasGameObjects b -> HasGameObjects b
replaceGameObjectsFrom { player, turrets, bullets, blasts } model =
    { model | player = player, turrets = turrets, bullets = bullets, blasts = blasts }


mapAccumEntitiesAsList : (Player -> List Entity -> ( a, List Entity )) -> Entities -> ( a, Entities )
mapAccumEntitiesAsList func model =
    func (getPlayerEntity_ model) (getEntityList_ model)
        |> Tuple.mapSecond (flip setEntityList_ model)


mapEntitiesAsList : (Player -> List Entity -> List Entity) -> Entities -> Entities
mapEntitiesAsList func (Entities initialPlayer list) =
    let
        entityToPlayer entity found =
            if found == Nothing then
                case entity of
                    PlayerE model ->
                        Just model

                    _ ->
                        Nothing

            else
                found

        player =
            List.foldl entityToPlayer Nothing list
                |> Maybe.withDefault initialPlayer
    in
    Entities initialPlayer (func player list)


getEntityList_ : Entities -> List Entity
getEntityList_ (Entities _ list) =
    list


getPlayerEntity_ : Entities -> Player
getPlayerEntity_ (Entities initialPlayer list) =
    let
        entityToPlayer entity found =
            if found == Nothing then
                case entity of
                    PlayerE player ->
                        Just player

                    _ ->
                        Nothing

            else
                found
    in
    List.foldl entityToPlayer Nothing list
        |> Maybe.withDefault initialPlayer


setEntityList_ : List Entity -> Entities -> Entities
setEntityList_ list (Entities initialPlayer _) =
    Entities initialPlayer list



-- GameState


type GameState
    = Running
    | GameOver Counter
    | Paused



-- Memory


type alias Turrets =
    List Turret


type alias Bullets =
    List Bullet


type DeathAnimationKind
    = TurretDeathAnim Turret
    | BulletDeathAnim Bullet
    | BlastDeathAnim Blast
    | PlayerDeathAnim Player


type alias Memory =
    HasDeathAnimations
        (HasGameObjects
            { rTicks : Float
            , state : GameState
            , stage : Int
            }
        )


allTurretsPositions : List Vec
allTurretsPositions =
    [ vec -1 -1, vec 1 1, vec 1 -1, vec -1 1 ]
        |> List.map (V.scaleBy 150)


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
            stageNumFromLevel ( 3, 1 )

        rTicks =
            0
    in
    { player = initPlayer
    , turrets = initTurretsForStage stage rTicks
    , bullets = []
    , blasts = []
    , deathAnimations = []
    , deathAnimationsClock = 0
    , stage = stage
    , state = Running
    , rTicks = rTicks
    }


spacePressed =
    G.freshKeyDown " "


gameOverDuration : number
gameOverDuration =
    60 * 3


updateMemory : G.Computer -> Memory -> Memory
updateMemory computer model =
    case model.state of
        Running ->
            if spacePressed computer then
                { model | state = Paused }

            else
                model
                    |> stepDeathAnimations
                    |> stepGameObjects model.rTicks computer
                    |> addNewDeathAnimations
                    |> incRunningTicks
                    |> handleGameOverOrNextStage

        GameOver counter ->
            let
                ( isComplete, newCounter ) =
                    Counter.step counter
            in
            if isComplete || spacePressed computer then
                initMemory

            else
                { model | state = GameOver newCounter }
                    |> stepDeathAnimations

        Paused ->
            if spacePressed computer then
                { model | state = Running }

            else
                model


type alias HasDeathAnimations a =
    { a | deathAnimationsClock : Float, deathAnimations : List DeathAnimation }


stepDeathAnimations : HasDeathAnimations a -> HasDeathAnimations a
stepDeathAnimations model =
    let
        clock =
            model.deathAnimationsClock
    in
    { model
        | deathAnimations =
            List.Extra.filterNot (.timer >> Timer.isDone clock) model.deathAnimations
        , deathAnimationsClock = clock + 1
    }


type alias HasGameObjects a =
    { a
        | player : Player
        , turrets : Turrets
        , bullets : Bullets
        , blasts : Blasts
    }


stepGameObjects :
    Float
    -> G.Computer
    -> HasGameObjects a
    -> ( List DeathAnimationKind, HasGameObjects a )
stepGameObjects rTicks computer model =
    let
        ( newDeathAnimationKinds, { player, turrets, bullets, blasts } ) =
            model
                |> entitiesFromHasGameObjects
                |> mapEntitiesAsList (stepEntity rTicks computer >> List.concatMap)
                |> mapEntitiesAsList (\_ -> foldMapSelf onEntityEntityCollision)
                |> mapAccumEntitiesAsList
                    (\player_ ->
                        List.Extra.mapAccuml
                            (\acc ->
                                handleEntityDeath rTicks player_ >> Tuple.mapFirst ((++) acc)
                            )
                            []
                            >> Tuple.mapSecond List.concat
                    )
                |> Tuple.mapSecond (flip replaceGameObjectsFromEntities model)
    in
    ( newDeathAnimationKinds
    , { model
        | player = player
        , turrets = turrets
        , bullets = bullets
        , blasts = blasts
      }
    )


stepEntity : Float -> G.Computer -> Player -> Entity -> List Entity
stepEntity rTicks computer player entity =
    let
        { mouse, screen } =
            computer
    in
    case entity of
        PlayerE model ->
            [ stepPlayer mouse model |> PlayerE ]

        TurretE turret ->
            stepTurret rTicks player turret
                |> Tuple.mapBoth (List.map BulletE) TurretE
                |> (\( l, i ) -> i :: l)

        BulletE bullet ->
            [ Bullet.stepBullet rTicks screen player bullet |> BulletE ]

        BlastE blast ->
            [ BlastE blast ]


handleEntityDeath : Float -> HasPosition x -> Entity -> ( List DeathAnimationKind, List Entity )
handleEntityDeath rTicks target entity =
    let
        noOp =
            ( [], [ entity ] )
    in
    case entity of
        BlastE bl ->
            ( [ BlastDeathAnim bl ], [] )

        PlayerE player ->
            if HasHealth.isDead player then
                ( [ PlayerDeathAnim player ], [ entity ] )

            else
                noOp

        TurretE turret ->
            if HasHealth.isDead turret then
                ( [ TurretDeathAnim turret ]
                , case turret.deathType of
                    NoBulletsOnDeathTurret ->
                        []

                    FiveBulletsOnDeathTurret ->
                        turretGenerateBulletForWeapon rTicks target GravityFive turret
                            |> List.map BulletE
                )

            else
                noOp

        BulletE bullet ->
            if HasHealth.isDead bullet then
                ( [ BulletDeathAnim bullet ], blastsFromBullet bullet |> List.map BlastE )

            else
                noOp



-- Collision


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
            onCircularCollisionMapBoth HasHealth.kill HasHealth.kill p t
                |> Tuple.mapBoth PlayerE TurretE

        ( TurretE _, PlayerE _ ) ->
            flipCallSwap ()

        ( PlayerE p, BulletE b ) ->
            case b.bulletType of
                Bullet.TimeBombBullet _ ->
                    onCircularCollisionMapBoth identity HasHealth.dec p b
                        |> Tuple.mapBoth PlayerE BulletE

                _ ->
                    onCircularCollisionMapBoth HasHealth.dec HasHealth.dec p b
                        |> Tuple.mapBoth PlayerE BulletE

        ( BulletE _, PlayerE _ ) ->
            flipCallSwap ()

        ( PlayerE p, BlastE bl ) ->
            onCircularCollisionMapBoth HasHealth.dec identity p bl
                |> Tuple.mapBoth PlayerE BlastE

        ( BlastE _, PlayerE _ ) ->
            flipCallSwap ()

        ( PlayerE _, PlayerE _ ) ->
            noOp

        ( TurretE t, BulletE b ) ->
            case b.bulletType of
                Bullet.TimeBombBullet _ ->
                    onCircularCollisionMapBoth identity HasHealth.dec t b
                        |> Tuple.mapBoth TurretE BulletE

                _ ->
                    onCircularCollisionMapBoth HasHealth.dec HasHealth.dec t b
                        |> Tuple.mapBoth TurretE BulletE

        ( BulletE _, TurretE _ ) ->
            flipCallSwap ()

        ( TurretE t, BlastE bl ) ->
            onCircularCollisionMapBoth HasHealth.dec identity t bl
                |> Tuple.mapBoth TurretE BlastE

        ( BlastE _, TurretE _ ) ->
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
    case bullet.bulletType of
        Bullet.TimeBombBullet _ ->
            [ { position = bullet.position
              , radius = bullet.radius * 15
              , color = G.green
              }
            ]

        _ ->
            []


deathAnimSetDuration : Float -> DeathAnimation -> DeathAnimation
deathAnimSetDuration duration model =
    { model | timer = Timer.setDuration duration model.timer }


addNewDeathAnimations : ( List DeathAnimationKind, HasDeathAnimations a ) -> HasDeathAnimations a
addNewDeathAnimations ( kinds, model ) =
    let
        toDeathAnimation : DeathAnimationKind -> DeathAnimation
        toDeathAnimation =
            DeathAnimation (Timer.start model.deathAnimationsClock 60)

        newDeathAnimations =
            model.deathAnimations ++ List.map toDeathAnimation kinds
    in
    { model | deathAnimations = newDeathAnimations }


handleGameOverOrNextStage : Memory -> Memory
handleGameOverOrNextStage model =
    let
        isPlayerDead =
            model.player |> HasHealth.isDead
    in
    if isPlayerDead then
        { model
            | deathAnimations = List.map (deathAnimSetDuration gameOverDuration) model.deathAnimations
            , state = GameOver (Counter.init gameOverDuration)
        }

    else
        let
            isStageComplete =
                List.isEmpty model.turrets
        in
        if isStageComplete then
            let
                nextStage =
                    model.stage + 1
            in
            { model
                | stage = nextStage
                , turrets = initTurretsForStage nextStage model.rTicks
            }

        else
            model


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

        screen =
            computer.screen

        alivePlayers =
            if model.player |> HasHealth.isAlive then
                [ model.player ]

            else
                []
    in
    List.map renderPlayer alivePlayers
        ++ renderDeathAnimations model
        ++ List.map (renderTurret rTicks) model.turrets
        ++ List.map (Bullet.renderBullet rTicks) model.bullets
        ++ viewGameState screen model.state
        ++ viewLevel screen model.stage


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


main : G.GameProgram Memory
main =
    G.game initMemory updateMemory viewMemory
