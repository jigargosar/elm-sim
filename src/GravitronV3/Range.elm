module GravitronV3.Range exposing (Range, break, init)

import List.Extra


type Range
    = Range Float Float


init : Float -> Float -> Range
init start end =
    Range start end


break : Float -> Range -> List Float
break parts (Range start end) =
    let
        interval =
            (end - start) / parts

        compareFn =
            if end > start then
                (>)

            else
                (<)

        func current =
            let
                next =
                    current + interval
            in
            if compareFn next end then
                Nothing

            else
                Just ( current, next )
    in
    List.Extra.unfoldr func start
