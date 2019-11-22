module GameOfLifeSvg2 exposing (..)

import Array exposing (Array)
import Random exposing (Generator)


type alias Model =
    { grid : Grid
    }


type alias Grid =
    { width : Int
    , height : Int
    , data : Array Cell
    }


randomGrid : Int -> Int -> Generator Grid
randomGrid width height =
    Random.list (width * height) (Random.weighted ( 20, Alive ) [ ( 80, Dead ) ])
        |> Random.map (Array.fromList >> Grid width height)


type Cell
    = Alive
    | Dead
