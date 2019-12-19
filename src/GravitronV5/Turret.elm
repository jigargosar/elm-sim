module GravitronV5.Turret exposing (Turret, TurretConfig(..), Weapon(..), initTurret)

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


initTurret : ( Number, Number ) -> TurretConfig -> Turret
initTurret ( x, y ) (TurretConfig id color weapon maxHP) =
    let
        turretRadius : Number
        turretRadius =
            25
    in
    Turret id
        Tag.TagTurret
        x
        y
        turretRadius
        color
        weapon
        (Timer.forTicks 160)
        (HP.withMax maxHP)
