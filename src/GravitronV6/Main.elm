module GravitronV6.Main exposing (main)

import GravitronV6.World as World exposing (World)
import Playground exposing (..)


init : World
init =
    World.init []


update _ =
    identity


view : Computer -> World -> List Shape
view _ =
    World.toList >> List.map viewEntity


viewEntity e =
    circle e.color e.r
        |> move e.x e.y


main =
    game view update init
