module GravitronV5.World exposing (Entity, World, WorldConfig, init, toList, update)

import GravitronV5.EntityConfig as EC exposing (EntityConfig, Move(..), Step(..))
import GravitronV5.Geom as Geom
import GravitronV5.Names exposing (Name)
import List.Extra
import Playground exposing (Color, Computer, Number, wave)


type World
    = World Int (List Entity)


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
    List.map (updateEntity worldConfig computer lst) lst
        |> foldResponses nid


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


updateEntity : WorldConfig -> Computer -> List Entity -> Entity -> Response
updateEntity =
    performSteps


performSteps : WorldConfig -> Computer -> List Entity -> Entity -> Response
performSteps wc computer allEntities =
    let
        one : Entity -> ( ( Response, Entity ), List Step )
        one e =
            List.Extra.mapAccuml (performStep wc computer allEntities) ( NoResponse, { e | steps = [] } ) e.steps

        two : ( ( Response, Entity ), List Step ) -> Response
        two ( ( res, e ), steps ) =
            Batch [ res, UpdateEntity { e | steps = steps, x = e.x + e.vx, y = e.y + e.vy } ]
    in
    one >> two


propEq : (c -> b) -> b -> c -> Bool
propEq func s b =
    func b == s


entityNamed : Name -> List Entity -> Maybe Entity
entityNamed name =
    List.Extra.find (propEq .name name)


performStep : WorldConfig -> Computer -> List Entity -> ( Response, Entity ) -> Step -> ( ( Response, Entity ), Step )
performStep wc { screen, time } allEntities ( response, e ) step =
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

                BounceInScreen bf ->
                    ( ( response, Geom.bounceVel bf screen e ), step )

                GravitateTo name ->
                    case entityNamed name allEntities of
                        Just { x, y } ->
                            ( ( response, Geom.gravitateVelTo x y e ), step )

                        Nothing ->
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


type Step
    = Move EC.Move
    | Fire { every : Int, elapsed : Int, name : Name }


type alias Entity =
    { name : Name
    , x : Number
    , y : Number
    , r : Number
    , vx : Number
    , vy : Number
    , color : Color
    , steps : List Step
    }


fromConfig : EntityConfig -> Entity
fromConfig =
    let
        fromConfigRec : EC.Rec -> Entity
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


toList : WorldConfig -> World -> List Entity
toList worldConfig (World _ lst) =
    lst
