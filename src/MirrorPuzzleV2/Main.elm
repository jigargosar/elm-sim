module MirrorPuzzleV2.Main exposing (main)

import MirrorPuzzleV2.Box as Box exposing (Box)
import MirrorPuzzleV2.Button as Button exposing (Button)
import MirrorPuzzleV2.Computer2 as Computer2 exposing (Computer2)
import MirrorPuzzleV2.Game2 as Game2
import MirrorPuzzleV2.Levels as Levels exposing (Levels)
import MirrorPuzzleV2.PuzzleGrid as PuzzleGrid
import Playground exposing (..)



-- Scenes: Puzzle Model


type alias PuzzleSceneModel =
    { grid : PuzzleGrid.Model
    , levels : Levels
    }


initPuzzleScene : Levels -> Scene
initPuzzleScene levels =
    PuzzleScene { grid = Levels.current levels, levels = levels }


initialPuzzleScene : Scene
initialPuzzleScene =
    initPuzzleScene Levels.initial


viewPuzzleScene : Computer2 -> PuzzleSceneModel -> List Shape
viewPuzzleScene computer { grid, levels } =
    let
        { mouse, time, screen } =
            computer

        isSolved =
            PuzzleGrid.isSolved grid
    in
    [ PuzzleGrid.view computer grid
    , words
        black
        ([ "Level "
         , String.fromInt (Levels.index levels + 1)
         , caseBool isSolved " solved" ""
         ]
            |> String.concat
        )
        |> moveY screen.top
        |> moveDown 50
    , puzzleSceneButtons screen
        |> List.map (Button.view mouse)
        |> group
    ]


caseBool : Bool -> c -> c -> c
caseBool bool true false =
    if bool then
        true

    else
        false


puzzleSceneButtons : Screen -> List (Button PuzzleButton)
puzzleSceneButtons screen =
    [ SelectLevel, NextLevel, PrevLevel ]
        |> List.map (getPuzzleBtn screen)


type PuzzleButton
    = SelectLevel
    | NextLevel
    | PrevLevel


getPuzzleBtn : Screen -> PuzzleButton -> Button PuzzleButton
getPuzzleBtn screen puzzleButton =
    case puzzleButton of
        SelectLevel ->
            Button.init SelectLevel "Select Level"
                |> Button.mapBox
                    (\box ->
                        box
                            |> Box.moveToTopLeft
                            |> Box.move ( screen.left, screen.top )
                            |> Box.moveRight 20
                            |> Box.moveDown 20
                    )

        NextLevel ->
            Button.init NextLevel "Next"
                |> Button.mapBox
                    (\box ->
                        box
                            |> Box.moveToTopRight
                            |> Box.move ( screen.right, screen.top )
                            |> Box.moveDown 20
                            |> Box.moveLeft 20
                    )

        PrevLevel ->
            Button.init PrevLevel "Prev"
                |> Button.mapBox
                    (\box ->
                        box
                            |> Box.moveToTopRight
                            |> Box.move ( screen.right, screen.top )
                            |> Box.moveDown 20
                            |> Box.moveLeft (100 + 20 + 20)
                    )


type alias Mouse =
    Computer2.Mouse



-- Scenes : Level Select


initialLevelSelect : Scene
initialLevelSelect =
    LevelSelect (Levels.initial |> Levels.count)


type alias LevelButtons =
    { top : Number
    , list : List (Button Int)
    }


initLevelButtons : Screen -> Int -> LevelButtons
initLevelButtons screen count =
    let
        lh =
            min 40 ((screen.height * 0.7) / toFloat count)

        hScale =
            0.8

        top =
            (toFloat count * lh - lh) / 2

        toY : Int -> Number
        toY n =
            top - (toFloat n * lh)

        height =
            lh * hScale

        width =
            150
    in
    { top = top
    , list =
        List.range 0 (count - 1)
            |> List.map
                (\n ->
                    Button.initWithBox n
                        ("Level " ++ String.fromInt n)
                        (Box.atOrigin width height
                            |> Box.moveY (toY n)
                        )
                )
    }


viewLevelSelect : Computer2 -> Int -> List Shape
viewLevelSelect computer levelCount =
    let
        lbs =
            initLevelButtons computer.screen levelCount
    in
    [ words black "Select Level"
        |> scale 1.5
        |> moveUp lbs.top
        |> moveUp 60
    , lbs.list
        |> List.map (Button.view computer.mouse)
        |> group
    ]



-- Scene


type Scene
    = LevelSelect Int
    | PuzzleScene PuzzleSceneModel



-- Game


type alias Mem =
    { scene : Scene }


init : Mem
init =
    { scene = initialPuzzleScene }


updateMem : Computer2 -> Mem -> Mem
updateMem computer mem =
    let
        msg =
            toMsg computer mem.scene
    in
    { mem | scene = updateScene computer msg mem.scene }


type Msg
    = LevelButtonClicked Int
    | PuzzleSceneBtnClicked PuzzleButton
    | UpdatePuzzleGrid
    | NoOp


toMsg : Computer2 -> Scene -> Msg
toMsg computer scene =
    let
        ev =
            computer.mouse.event
    in
    case scene of
        LevelSelect levelCount ->
            initLevelButtons computer.screen levelCount
                |> .list
                |> Button.findClicked ev
                |> Maybe.map LevelButtonClicked
                |> Maybe.withDefault NoOp

        PuzzleScene _ ->
            puzzleSceneButtons computer.screen
                |> Button.findClicked ev
                |> Maybe.map PuzzleSceneBtnClicked
                |> Maybe.withDefault UpdatePuzzleGrid


updateScene : Computer2 -> Msg -> Scene -> Scene
updateScene computer msg scene =
    case ( msg, scene ) of
        ( LevelButtonClicked idx, _ ) ->
            Levels.fromIndex idx |> initPuzzleScene

        ( PuzzleSceneBtnClicked btn, PuzzleScene model ) ->
            case btn of
                SelectLevel ->
                    initialLevelSelect

                NextLevel ->
                    Levels.next model.levels
                        |> Maybe.map initPuzzleScene
                        |> Maybe.withDefault scene

                PrevLevel ->
                    Levels.prev model.levels
                        |> Maybe.map initPuzzleScene
                        |> Maybe.withDefault scene

        ( UpdatePuzzleGrid, PuzzleScene model ) ->
            PuzzleScene { model | grid = PuzzleGrid.update computer model.grid }

        _ ->
            scene


viewMem : Computer2 -> Mem -> List Shape
viewMem computer mem =
    case mem.scene of
        LevelSelect levelCount ->
            viewLevelSelect computer levelCount

        PuzzleScene puzzle ->
            viewPuzzleScene computer puzzle


main =
    Game2.game2 viewMem updateMem init
