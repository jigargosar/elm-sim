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


makeSubLevel : List (Entity -> Entity) -> List Entity
makeSubLevel funcList =
    List.map2 (<|) funcList turretTemplates


basic1 : Entity -> Entity
basic1 =
    identity


basic2 : Entity -> Entity
basic2 t =
    { t | color = blue, maxHP = 2 }


levels =
    [ [ makeSubLevel [ basic1 ]
      , makeSubLevel [ basic1, basic1 ]
      , makeSubLevel [ basic1, basic1, basic2 ]
      , makeSubLevel (List.repeat 4 basic2)
      ]
    ]


type alias Mem =
    { level : Level
    , world : World
    }


init : Mem
init =
    let
        lev =
            ( 0, 0 )
    in
    Mem lev (initWorld lev)


getTurretsForLevel : Level -> List Entity
getTurretsForLevel ( major, minor ) =
    let
        majorMax =
            List.length levels

        minorMax =
            5
    in
    List.Extra.getAt (modBy majorMax major) levels
        |> Maybe.andThen (List.Extra.getAt (modBy minorMax minor))
        |> Maybe.withDefault turretTemplates


initWorld : Level -> World
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


type alias Level =
    ( Int, Int )


nextLevel : Level -> Level
nextLevel ( a, b ) =
    if b + 1 >= 5 then
        ( modBy (List.length levels) a + 1, 0 )

    else
        ( a, b + 1 )


afterUpdateHook : Level -> List Entity -> ( Level, List World.NewEntity )
afterUpdateHook lev =
    List.Extra.count (propEq .name (name Turret))
        >> (\tc ->
                if tc == 0 then
                    let
                        nextLev =
                            nextLevel lev
                    in
                    ( nextLev, List.map World.newEntity (getTurretsForLevel nextLev) )

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
