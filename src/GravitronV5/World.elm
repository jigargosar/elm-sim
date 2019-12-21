module GravitronV5.World exposing (Entity, World, WorldConfig, init, toList, update)

import GravitronV5.EntityConfig as EC exposing (EntityConfig, Move(..), Step(..))
import Playground exposing (Color, Computer, Number, wave)


type World name
    = World Int (List (Entity name))


type alias WorldConfig name =
    { singletonNames : List name, configOf : name -> EntityConfig name }


init : WorldConfig name -> List (EntityConfig name) -> World name
init _ initialEntityConfigList =
    let
        entityList =
            initialEntityConfigList
                |> List.map fromConfig
    in
    World (List.length entityList) entityList


update : WorldConfig name -> Computer -> World name -> World name
update worldConfig computer (World nid lst) =
    World nid (List.map (updateEntity computer) lst)


updateEntity : Computer -> Entity name -> Entity name
updateEntity { screen, time } =
    let
        stepMove : Move name -> Entity name -> Entity name
        stepMove move e =
            case move of
                RandomWalker ->
                    withXY
                        ( wave screen.left screen.right 6 time
                        , wave screen.top screen.bottom 8 time
                        )
                        e

                _ ->
                    e

        prependStep : Step name -> Entity name -> Entity name
        prependStep step e =
            { e | steps = step :: e.steps }

        stepEntity : Step name -> Entity name -> Entity name
        stepEntity step e =
            case step of
                Move move ->
                    stepMove move e |> prependStep step

                Fire r ->
                    let
                        newR =
                            if r.elapsed > r.every then
                                { r | elapsed = 0 }

                            else
                                { r | elapsed = r.elapsed + 1 }
                    in
                    prependStep (Fire newR) e

        revStep : Entity name -> Entity name
        revStep e =
            { e | steps = List.reverse e.steps }
    in
    \e ->
        List.foldl stepEntity { e | steps = [] } e.steps
            |> revStep


withXY : ( Number, Number ) -> { c | x : Number, y : Number } -> { c | x : Number, y : Number }
withXY ( x, y ) e =
    { e | x = x, y = y }


type Step name
    = Move (EC.Move name)
    | Fire { every : Int, elapsed : Int, name : name }


type alias Entity name =
    { name : name
    , x : Number
    , y : Number
    , r : Number
    , vx : Number
    , vy : Number
    , color : Color
    , steps : List (Step name)
    }


fromConfig : EntityConfig name -> Entity name
fromConfig =
    let
        fromConfigRec : EC.Rec name -> Entity name
        fromConfigRec { name, x, y, r, vx, vy, color, step } =
            { name = name
            , x = x
            , y = y
            , r = r
            , vx = vx
            , vy = vy
            , color = color
            , steps =
                List.map
                    (\s ->
                        case s of
                            EC.Move m ->
                                Move m

                            EC.Fire n ->
                                Fire { every = 60, name = n, elapsed = 0 }
                    )
                    step
            }
    in
    EC.toRec >> fromConfigRec


toList : WorldConfig name -> World name -> List (Entity name)
toList worldConfig (World _ lst) =
    lst
