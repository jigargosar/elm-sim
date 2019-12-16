module GravitronV4.Main exposing (main)

import Playground exposing (..)


type alias Player =
    { x : Number
    , y : Number
    }


type alias Turret =
    { x : Number
    , y : Number
    , color : Color
    }


type alias Bullet =
    { x : Number
    , y : Number
    , vx : Number
    , vy : Number
    }


type alias Memory =
    { player : Player
    , turrets : List Turret
    , bullets : List Bullet
    , clock : Int
    }


viewMemory : Computer -> Memory -> List Shape
viewMemory _ { player, turrets } =
    [ viewPlayer player
    , viewTurrets turrets
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


updateMemory : Computer -> Memory -> Memory
updateMemory { time } ({ turrets, clock } as mem) =
    { mem
        | clock = clock + 1
    }


initialMemory : Memory
initialMemory =
    { player = Player 0 0
    , turrets = initTurrets [ red, red, blue, orange ]
    , bullets = []
    , clock = 0
    }


initTurrets : List Color -> List Turret
initTurrets =
    let
        positions =
            [ ( -1, 1 ), ( 1, -1 ), ( 1, 1 ), ( -1, -1 ) ]

        factor =
            150

        initTurret ( x, y ) =
            Turret (x * factor) (y * factor)
    in
    List.map2 initTurret positions


main =
    Playground.game viewMemory updateMemory initialMemory
