module GravitronV4.Main exposing (main)

import Playground exposing (..)


type Counter
    = Counter Int Int


initCt : Int -> Counter
initCt =
    Counter 0 << max 1


stepCt : Counter -> Counter
stepCt (Counter n mx) =
    Counter (n + 1 |> modBy mx) mx


isDone : Counter -> Bool
isDone (Counter n mx) =
    n == mx - 1


pct : Counter -> Float
pct (Counter n mx) =
    toFloat n / toFloat mx


type alias Player =
    { x : Number
    , y : Number
    }


type alias Turret =
    { ct : Counter
    , x : Number
    , y : Number
    , color : Color
    }


type alias Bullet =
    { x : Number
    , y : Number
    , vx : Number
    , vy : Number
    }


type alias Mem =
    { player : Player
    , turrets : List Turret
    , bullets : List Bullet
    }


initialMemory : Mem
initialMemory =
    { player = Player 0 0
    , turrets = initTurrets [ red, red, blue, orange ]
    , bullets = []
    }


initTurrets : List Color -> List Turret
initTurrets =
    let
        positions =
            [ ( -1, 1 ), ( 1, -1 ), ( 1, 1 ), ( -1, -1 ) ]

        factor =
            150

        initTurret ( x, y ) =
            Turret (initCt 60) (x * factor) (y * factor)
    in
    List.map2 initTurret positions



--  Update


updateMemory : Computer -> Mem -> Mem
updateMemory { time } ({ turrets } as mem) =
    mem
        |> stepBullets
        |> stepFireTurretBullets
        |> stepTurrets


stepBullets : Mem -> Mem
stepBullets mem =
    let
        stepBullet : Bullet -> Bullet
        stepBullet b =
            { b | x = b.x + b.vx, y = b.y + b.vy }
    in
    { mem | bullets = List.map stepBullet mem.bullets }


stepTurrets : Mem -> Mem
stepTurrets mem =
    let
        stepTurret : Turret -> Turret
        stepTurret t =
            { t | ct = stepCt t.ct }
    in
    { mem | turrets = List.map stepTurret mem.turrets }


stepFireTurretBullets : Mem -> Mem
stepFireTurretBullets mem =
    let
        initBullet : Number -> Number -> Bullet
        initBullet x y =
            Bullet x y 1 1

        fireBulletOnCounter t =
            if isDone t.ct then
                (::) (initBullet t.x t.y)

            else
                identity
    in
    { mem
        | bullets =
            mem.bullets
                ++ List.foldl fireBulletOnCounter [] mem.turrets
                |> List.take 500
    }


viewMemory : Computer -> Mem -> List Shape
viewMemory _ { player, turrets, bullets } =
    [ viewPlayer player
    , viewTurrets turrets
    , viewBullets bullets
    ]


viewPlayer : Player -> Shape
viewPlayer { x, y } =
    circle green 20
        |> move x y


viewTurrets : List Turret -> Shape
viewTurrets =
    let
        viewTurret { x, y, color } =
            circle color 25
                |> move x y
    in
    List.map viewTurret >> group


viewBullets : List Bullet -> Shape
viewBullets =
    let
        viewBullet { x, y } =
            circle black 10
                |> move x y
    in
    List.map viewBullet >> group


main =
    Playground.game viewMemory updateMemory initialMemory
