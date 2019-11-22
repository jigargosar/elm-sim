module GameOfLifeSvg2 exposing (..)

import Array exposing (Array)
import Random exposing (Generator, Seed)


type alias Model =
    { grid : Grid
    , seed : Seed
    }


type alias Grid =
    { width : Int
    , height : Int
    , data : Array Cell
    }


gridGenerator : Int -> Int -> Generator Grid
gridGenerator width height =
    Random.list (width * height) (Random.weighted ( 20, Alive ) [ ( 80, Dead ) ])
        |> Random.map (Array.fromList >> Grid width height)


type Cell
    = Alive
    | Dead
