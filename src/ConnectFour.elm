module ConnectFour exposing (..)

import Array exposing (Array)
import Playground exposing (..)


type Cell
    = Red
    | Yellow
    | Empty


gridWidth =
    10


gridHeight =
    10


type alias Grid =
    Array (Array Cell)


initGrid : Grid
initGrid =
    Array.repeat gridHeight (Array.repeat gridWidth Empty)


cellAt : Int -> Int -> Grid -> Cell
cellAt x y grid =
    Array.get x grid |> Maybe.andThen (Array.get y) |> Maybe.withDefault Empty


main =
    picture
