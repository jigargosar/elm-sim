module GravitronV6.Main exposing (main)

import GravitronV6.Entity as Entity exposing (AliveStep(..), Entity, NewEntity, Phase(..), withColor, withHP)
import GravitronV6.LevelId as LevelId exposing (LevelId, MajorLevel, MinorLevel)
import GravitronV6.World as World exposing (World)
import List.Extra
import Playground exposing (..)
import PointFree exposing (propEq)



-- TypeSafe Names


type Name
    = Player
    | Turret
    | Bullet


name : Name -> String
name n =
    case n of
        Player ->
            "Player"

        Turret ->
            "Turret"

        Bullet ->
            "Bullet"


names : List Name -> List String
names =
    List.map name



-- Entity Templates


defaultTmpl : Entity
defaultTmpl =
    Entity.default


playerTemplate : Entity
playerTemplate =
    { defaultTmpl | name = name Player, r = 20, color = green, aliveSteps = [ WalkRandomly ] }


bulletTemplate : Entity
bulletTemplate =
    { defaultTmpl
        | name = name Bullet
        , r = 10
        , color = charcoal
        , vx = 1
        , vy = 1
        , aliveSteps =
            [ GravitateTo (name Player)
            , BounceInScreen 0.5
            , DieOnCollisionWith (names [ Player, Turret, Bullet ])
            ]
    }



-- Turret Templates


turretTemplate : Entity
turretTemplate =
    { defaultTmpl
        | name = name Turret
        , r = 25
        , color = red
        , phase = SpawningPhase { elapsed = 0, duration = 60 }
        , aliveSteps =
            [ Fire
                { elapsed = 0
                , didTrigger = False
                , every = 60
                , towards = name Player
                , template = bulletTemplate
                , speed = 3
                }
            , ReceiveCollisionDamageFrom (names [ Bullet ])
            ]
    }


turretPositions : List ( number, number )
turretPositions =
    let
        f =
            (*) 150
    in
    [ ( -1, 1 ), ( 1, -1 ), ( 1, 1 ), ( -1, -1 ) ] |> List.map (Tuple.mapBoth f f)


turretTemplates : List Entity
turretTemplates =
    List.map (\( x, y ) -> { turretTemplate | x = x, y = y }) turretPositions


basic1 : Entity -> Entity
basic1 =
    identity


basic2 : Entity -> Entity
basic2 =
    withColor blue >> withHP 2


basic2RevengeWanderer : Entity -> Entity
basic2RevengeWanderer =
    withColor blue
        >> withHP 2
        >> (\e -> { e | vx = 1, vy = 1, aliveSteps = Wanderer :: e.aliveSteps })


tripleG : Entity -> Entity
tripleG =
    withColor green >> withHP 3


heatSinkShooter : Entity -> Entity
heatSinkShooter =
    withColor orange >> withHP 5



-- Level Config


makeSubLevel : List (Entity -> Entity) -> List Entity
makeSubLevel funcList =
    List.map2 (<|) funcList turretTemplates


levels : List MajorLevel
levels =
    [ [ makeSubLevel [ basic1 ]
      , makeSubLevel [ basic1, basic1 ]
      , makeSubLevel [ basic1, basic2 ]
      , makeSubLevel [ basic1, basic1, basic2 ]
      , makeSubLevel (List.repeat 4 basic2)
      ]
    , [ makeSubLevel [ tripleG ]
      , makeSubLevel [ basic1, tripleG, basic2 ]
      , makeSubLevel [ tripleG, basic2RevengeWanderer ]
      , makeSubLevel [ basic2RevengeWanderer, basic2RevengeWanderer, tripleG ]
      , makeSubLevel [ tripleG, heatSinkShooter, basic2RevengeWanderer ]
      ]
    ]



-- Memory


type alias Mem =
    { lid : LevelId
    , world : World
    }


init : Mem
init =
    let
        lid =
            ( 0, 0 )
    in
    Mem lid (initWorld lid)


getTurretsFor : LevelId -> List Entity
getTurretsFor lid =
    LevelId.getMinorLevel levels lid |> Maybe.withDefault turretTemplates


initWorld : LevelId -> World
initWorld lid =
    (playerTemplate
        :: getTurretsFor lid
    )
        |> List.map Entity.new
        |> World.init


update : Computer -> Mem -> Mem
update computer mem =
    let
        ( lid, world ) =
            World.stepAll (afterUpdateHook mem.lid) computer mem.world
    in
    { mem
        | world = world
        , lid = lid
    }


afterUpdateHook : LevelId -> List Entity -> ( LevelId, List NewEntity )
afterUpdateHook lid =
    List.Extra.count (propEq .name (name Turret))
        >> (\tc ->
                if tc == 0 then
                    let
                        nextLid =
                            LevelId.next levels lid
                    in
                    ( nextLid, List.map Entity.new (getTurretsFor nextLid) )

                else
                    ( lid, [] )
           )


view : Computer -> Mem -> List Shape
view _ =
    .world >> World.toList >> List.indexedMap viewEntity


viewEntity : Int -> Entity -> Shape
viewEntity idx e =
    toShape idx e
        |> applyPhaseTransform e.phase
        |> move e.x e.y


applyPhaseTransform : Phase -> Shape -> Shape
applyPhaseTransform phase shape =
    case phase of
        SpawningPhase sm ->
            let
                progress =
                    sm.elapsed / sm.duration
            in
            shape
                |> fade (progress + 0.1)
                |> scale progress

        ReadyPhase ->
            shape

        DyingPhase dm ->
            let
                progress =
                    dm.elapsed / dm.duration
            in
            shape
                |> fade (1 - progress)
                |> scale (1 + progress * 0.5)


toShape : Int -> Entity -> Shape
toShape idx e =
    group
        [ circle e.color e.r
        , ("z-" ++ String.fromInt idx)
            |> words black
            |> moveDown (e.r + 10)
        , if name Turret == e.name then
            group
                [ String.fromInt (round e.currentHP)
                    |> words white
                ]

          else
            group []
        ]


main =
    game view update init
