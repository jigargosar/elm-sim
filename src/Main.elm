module Main exposing (main)

import MainStage exposing (group, rect, stage)


main =
    stage ( 600, 600 )
        [ group [ rect 300 400 ]
        ]
