module GravitronV5.Turret exposing (Turret, TurretConfig(..), Weapon(..), init, initAt)

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


type TurretConfig
    = TurretConfig Id Color Weapon Int


turretRadius : Number
turretRadius =
    25


initAt : ( Number, Number ) -> TurretConfig -> Turret
initAt ( x, y ) (TurretConfig id color weapon maxHP) =
    Turret id
        Tag.TagTurret
        x
        y
        turretRadius
        color
        weapon
        (Timer.forTicks 160)
        (HP.withMax maxHP)


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
