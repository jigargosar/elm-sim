module GravitronV5.Turret exposing (Turret, Weapon(..), init, setOrigin)

import GravitronV5.HP as HP exposing (HP)
import GravitronV5.Id exposing (Id)
import GravitronV5.Tag as Tag exposing (Tag)
import GravitronV5.Timer as Timer exposing (Timer)
import Playground exposing (..)


type alias Turret =
    { id : Id
    , tag : Tag
    , x : Number
    , y : Number
    , r : Number
    , color : Color
    , weapon : Weapon
    , trigger : Timer
    , hp : HP
    }


type Weapon
    = BulletWeapon
    | TimeBombWeapon


turretRadius : Number
turretRadius =
    25


init : Id -> Color -> Weapon -> Int -> Turret
init id color weapon maxHP =
    Turret id
        Tag.TagTurret
        0
        0
        turretRadius
        color
        weapon
        (Timer.forTicks 160)
        (HP.withMax maxHP)


setOrigin : Number -> Number -> Turret -> Turret
setOrigin x y turret =
    { turret | x = x, y = y }
