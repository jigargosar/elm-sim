module GravitronV5.World exposing (Entity, World, WorldConfig, init, toList, update)

import GravitronV5.EntityConfig as EC exposing (EntityConfig, Move(..), Step(..))
import GravitronV5.Geom as Geom
import GravitronV5.Names exposing (Name)
import List.Extra
import Playground exposing (Color, Computer, Number, wave)


type World
    = World Int (List Entity)


type alias Entity =
    { name : Name
    , x : Number
    , y : Number
    , r : Number
    , vx : Number
    , vy : Number
    , color : Color
    , preSteps : List PreStep
    , steps : List Step
    }


type PreStep
    = ReceiveCollisionDamage (List Name)
    | DieOnCollision (List Name)
    | DieOnTimeout Int


type Step
    = Move EC.Move
    | Fire { every : Int, elapsed : Int, name : Name }


fromConfig : EntityConfig -> Entity
fromConfig =
    let
        fromConfigRec : EC.Rec -> Entity
        fromConfigRec { name, x, y, r, vx, vy, color, preSteps, steps } =
            { name = name
            , x = x
            , y = y
            , r = r
            , vx = vx
            , vy = vy
            , color = color
            , preSteps =
                List.map
                    (\s ->
                        case s of
                            EC.DieOnCollision ns ->
                                DieOnCollision ns

                            EC.ReceiveCollisionDamage names ->
                                ReceiveCollisionDamage names

                            EC.DieOnTimeout int ->
                                DieOnTimeout int
                    )
                    preSteps
            , steps =
                List.map
                    (\s ->
                        case s of
                            EC.Move m ->
                                Move m

                            EC.Fire n ->
                                Fire { every = 60, name = n, elapsed = 0 }
                    )
                    steps
            }
    in
    EC.toRec >> fromConfigRec


type alias WorldConfig =
    { singletonNames : List Name, configOf : Name -> EntityConfig }


init : WorldConfig -> List EntityConfig -> World
init _ initialEntityConfigList =
    let
        entityList =
            initialEntityConfigList
                |> List.map fromConfig
    in
    World (List.length entityList) entityList


update : WorldConfig -> Computer -> World -> World
update worldConfig computer (World nid lst) =
    let
        env : Env
        env =
            Env worldConfig computer lst
    in
    List.map (updateEntity env) lst
        |> foldResponses nid


type Env
    = Env WorldConfig Computer (List Entity)


propEq : (c -> b) -> b -> c -> Bool
propEq func s b =
    func b == s


entityNamed : Name -> List Entity -> Maybe Entity
entityNamed name =
    List.Extra.find (propEq .name name)


setXY : ( Number, Number ) -> { c | x : Number, y : Number } -> { c | x : Number, y : Number }
setXY ( x, y ) e =
    { e | x = x, y = y }


type Response
    = UpdateEntity Entity
    | NewEntity EntityConfig
    | Batch (List Response)
    | NoResponse


foldResponses : Int -> List Response -> World
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


updateEntity : Env -> Entity -> Response
updateEntity env =
    performPreSteps env >> performSteps env


performPreSteps : Env -> Entity -> Entity
performPreSteps env =
    let
        one : Entity -> ( Entity, List PreStep )
        one e =
            List.Extra.mapAccuml (performPreStep env) { e | preSteps = [] } e.preSteps

        two ( e, preSteps ) =
            { e | preSteps = preSteps }
    in
    one >> two


performPreStep : Env -> Entity -> PreStep -> ( Entity, PreStep )
performPreStep =
    Debug.todo ""


performSteps : Env -> Entity -> Response
performSteps env =
    let
        one : Entity -> ( ( Response, Entity ), List Step )
        one e =
            List.Extra.mapAccuml (performStep env) ( NoResponse, { e | steps = [] } ) e.steps

        two : ( ( Response, Entity ), List Step ) -> Response
        two ( ( res, e ), steps ) =
            Batch [ res, UpdateEntity { e | steps = steps, x = e.x + e.vx, y = e.y + e.vy } ]
    in
    one >> two


performStep : Env -> ( Response, Entity ) -> Step -> ( ( Response, Entity ), Step )
performStep (Env { configOf } { screen, time } entityList) ( response, e ) step =
    case step of
        Move move ->
            case move of
                RandomWalker ->
                    let
                        newE =
                            setXY
                                ( wave screen.left screen.right 6 time
                                , wave screen.top screen.bottom 8 time
                                )
                                e
                    in
                    ( ( response, newE ), step )

                BounceInScreen bf ->
                    ( ( response, Geom.bounceVel bf screen e ), step )

                GravitateTo name ->
                    case entityNamed name entityList of
                        Just { x, y } ->
                            ( ( response, Geom.gravitateVelTo x y e ), step )

                        Nothing ->
                            ( ( response, e ), step )

        Fire fire ->
            let
                newConfig name =
                    configOf name
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


toList : WorldConfig -> World -> List Entity
toList worldConfig (World _ lst) =
    lst
