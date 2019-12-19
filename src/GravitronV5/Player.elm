module GravitronV5.Player exposing (Player, init, origin, toTaggedCircle, updatePlayer, view)

import GravitronV5.Geom as Geom
import GravitronV5.Id as Id exposing (Id)
import GravitronV5.Tag as Tag exposing (Tag)
import GravitronV5.TaggedCircle as TaggedCircle exposing (TaggedCircle)
import Playground exposing (..)


type Player
    = Player Model


type alias Model =
    { id : Id
    , tag : Tag
    , x : Number
    , y : Number
    , r : Number
    , vx : Number
    , vy : Number
    }


init : Number -> Number -> Player
init x y =
    let
        initialPlayerRadius =
            20
    in
    Model Id.Player Tag.TagPlayer x y initialPlayerRadius 0 0
        |> Player


origin : Player -> ( Number, Number )
origin (Player { x, y }) =
    ( x, y )


toTaggedCircle : Player -> TaggedCircle
toTaggedCircle =
    unwrap >> TaggedCircle.toTaggedCircle


updatePlayer : Screen -> Mouse -> Time -> Player -> Player
updatePlayer screen mouse time =
    let
        randomVel : Model -> Model
        randomVel ({ vx, vy } as p) =
            let
                ( dx, dy ) =
                    ( wave -100 100 11 time, wave -300 300 21 time )
                        |> toPolar
                        |> Tuple.mapFirst ((*) 0.0005)
                        |> fromPolar
            in
            { p | vx = vx + dx, vy = vy + dy }
    in
    map
        (if mouse.down then
            Geom.springToMouse mouse >> Geom.addVelToPos

         else
            randomVel >> Geom.bounceVel 1 screen >> Geom.addVelToPos
        )


unwrap (Player model) =
    model


map func =
    unwrap >> func >> Player


view : Player -> Shape
view (Player { x, y, r }) =
    circle green r
        |> move x y
