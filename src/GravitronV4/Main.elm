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


type alias Memory =
    { player : Player
    , turrets : List Turret
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


updateMemory _ =
    identity


initialMemory : Memory
initialMemory =
    { player = Player 0 0
    , turrets = initTurrets [ red, red, blue, orange ]
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
