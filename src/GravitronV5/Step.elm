module GravitronV5.Step exposing (..)

import GravitronV5.EntityConfig as EC
import GravitronV5.Names exposing (Name)
import Playground exposing (Number)


type Step
    = Move EC.Move
    | Fire { every : Int, named : Name, towards : Name, speed : Number } Int
