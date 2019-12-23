module GravitronV6.Main exposing (main)

import GravitronV6.Entity as Entity exposing (AliveStep(..), Entity, Phase(..))
import GravitronV6.World as World exposing (World)
import List.Extra
import Playground exposing (..)
import PointFree exposing (propEq)


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


default : Entity
default =
    Entity.default


bulletTemplate : Entity
bulletTemplate =
    { default
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


turretTemplate =
    { default
        | name = name Turret
        , r = 25
        , color = red
        , phase = Spawning { elapsed = 0, duration = 60 }
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


levels =
    [ turretTemplates
    , turretTemplates |> List.map (\t -> { t | color = blue, maxHP = 2 })
    ]


type alias Mem =
    { level : Int
    , world : World
    }


init : Mem
init =
    let
        lev =
            0
    in
    Mem lev (initWorld lev)


getTurretsForLevel : Int -> List Entity
getTurretsForLevel level =
    let
        levelCt =
            List.length levels
    in
    List.drop (modBy levelCt level) levels |> List.head |> Maybe.withDefault turretTemplates


initWorld : Int -> World
initWorld level =
    ({ default | name = name Player, r = 20, color = green, aliveSteps = [ WalkRandomly ] }
        :: getTurretsForLevel level
    )
        |> List.map World.newEntity
        |> World.init


update : Computer -> Mem -> Mem
update computer mem =
    let
        ( lev, world ) =
            World.update (afterUpdateHook mem.level) computer mem.world
    in
    { mem
        | world = world
        , level = lev
    }


afterUpdateHook : Int -> List Entity -> ( Int, List World.NewEntity )
afterUpdateHook lev =
    List.Extra.count (propEq .name (name Turret))
        >> (\tc ->
                if tc == 0 then
                    ( lev + 1, List.map World.newEntity (getTurretsForLevel (lev + 1)) )

                else
                    ( lev, [] )
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
        Spawning sm ->
            let
                progress =
                    sm.elapsed / sm.duration
            in
            shape
                |> fade (progress + 0.1)
                |> scale progress

        Alive ->
            shape

        Dying dm ->
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
