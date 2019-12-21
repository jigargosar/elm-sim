module GravitronV5.World exposing (Entity, World, WorldConfig, init, toList, update)

import GravitronV5.EntityConfig as EC exposing (EntityConfig, Move(..), Step(..))
import List.Extra
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
    List.map (updateEntity worldConfig computer) lst
        |> foldResponses nid


type Response name
    = UpdateEntity (Entity name)
    | NewEntity (EntityConfig name)
    | Batch (List (Response name))
    | NoResponse


foldResponses : Int -> List (Response name) -> World name
foldResponses =
    let
        foldOne r ((World nid acc) as world) =
            case r of
                UpdateEntity e ->
                    World nid (e :: acc)

                NewEntity ec ->
                    World nid (fromConfig ec :: acc)

                Batch rLst ->
                    List.foldl foldOne world rLst

                NoResponse ->
                    world

        revLst (World nid lst) =
            World nid (List.reverse lst)
    in
    \nid -> List.foldl foldOne (World nid []) >> revLst


updateEntity : WorldConfig name -> Computer -> Entity name -> Response name
updateEntity =
    performSteps


performSteps : WorldConfig name -> Computer -> Entity name -> Response name
performSteps wc computer =
    let
        one : Entity name -> ( ( Response name, Entity name ), List (Step name) )
        one e =
            List.Extra.mapAccuml (performStep wc computer) ( NoResponse, { e | steps = [] } ) e.steps

        two : ( ( Response name, Entity name ), List (Step name) ) -> Response name
        two ( ( res, e ), steps ) =
            Batch [ res, UpdateEntity { e | steps = steps, x = e.x + e.vx, y = e.y + e.vy } ]
    in
    one >> two


performStep : WorldConfig name -> Computer -> ( Response name, Entity name ) -> Step name -> ( ( Response name, Entity name ), Step name )
performStep wc { screen, time } ( response, e ) step =
    case step of
        Move move ->
            case move of
                RandomWalker ->
                    let
                        newE =
                            withXY
                                ( wave screen.left screen.right 6 time
                                , wave screen.top screen.bottom 8 time
                                )
                                e
                    in
                    ( ( response, newE ), step )

                _ ->
                    ( ( response, e ), step )

        Fire fire ->
            let
                newConfig name =
                    wc.configOf name
                        |> EC.map
                            (\ec ->
                                let
                                    ( vx, vy ) =
                                        ( 1, 1 )
                                in
                                { ec | vx = vx, vy = vy }
                            )

                ( newResponse, newFire ) =
                    if fire.elapsed > fire.every then
                        ( Batch [ response, NewEntity (newConfig fire.name) ], { fire | elapsed = 0 } )

                    else
                        ( response, { fire | elapsed = fire.elapsed + 1 } )
            in
            ( ( newResponse, e ), Fire newFire )


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
