module List2d exposing (List2d, height, maxWidth, toDict)

import Dict exposing (Dict)
import List.Extra
import Number2 exposing (Int2)
import PointFree exposing (flip)


type alias List2d a =
    List (List a)


indexedFoldl : (Int2 -> a -> b -> b) -> b -> List2d a -> b
indexedFoldl func =
    List.Extra.indexedFoldl
        (\y ->
            List.Extra.indexedFoldl (\x -> func ( x, y )) |> flip
        )


maxWidth : List2d a -> Int
maxWidth =
    List.map List.length >> List.maximum >> Maybe.withDefault 0


height : List2d a -> Int
height =
    List.length


toDict : List2d v -> Dict Int2 v
toDict =
    indexedFoldl Dict.insert Dict.empty
