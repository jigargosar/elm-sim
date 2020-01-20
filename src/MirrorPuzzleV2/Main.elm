module MirrorPuzzleV2.Main exposing (main)

import List.Extra
import MirrorPuzzleV2.Box as Box exposing (Box)
import MirrorPuzzleV2.Button as Button exposing (Button)
import MirrorPuzzleV2.Computer2 as Computer2 exposing (Computer2)
import MirrorPuzzleV2.Game2 as Game2
import MirrorPuzzleV2.Levels as Levels exposing (Levels)
import MirrorPuzzleV2.MouseEvent exposing (MouseEvent(..))
import MirrorPuzzleV2.PuzzleGrid as PuzzleGrid
import Number2 exposing (Float2)
import Playground exposing (..)
import Playground.Extra exposing (..)



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
    , viewPuzzleSceneButtons mouse screen
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


viewPuzzleSceneButtons : Mouse -> Screen -> Shape
viewPuzzleSceneButtons mouse screen =
    puzzleSceneButtons screen
        |> List.map (Button.view mouse)
        |> group



-- Scenes : Level Select


initialLevelSelect : Scene
initialLevelSelect =
    LevelSelect (Levels.initial |> Levels.count)


type alias LevelButtons =
    { top : Number
    , list : List Box
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
                    Box.atOrigin width height
                        |> Box.moveY (toY n)
                )
    }


levelButtonIdxAt : Float2 -> Screen -> Int -> Maybe Int
levelButtonIdxAt mouse screen levelCount =
    initLevelButtons screen levelCount
        |> .list
        |> List.Extra.findIndex (Box.contains mouse)


renderLevelButtons : Float2 -> LevelButtons -> Shape
renderLevelButtons mouse lbs =
    lbs.list
        |> List.indexedMap
            (\levelIdx rect ->
                renderButton
                    mouse
                    ("Level " ++ String.fromInt (levelIdx + 1))
                    rect
            )
        |> group


renderButton : Float2 -> String -> Box -> Shape
renderButton mouse text rect =
    buttonShape (Box.contains mouse rect)
        (Box.dimensions rect)
        text
        |> move2 (Box.center rect)


buttonShape : Bool -> ( Number, Number ) -> String -> Shape
buttonShape hover ( w, h ) text =
    let
        thickness =
            3
    in
    [ rectangle black w h
    , rectangle
        (if hover then
            lightPurple

         else
            white
        )
        (w - thickness)
        (h - thickness)
    , words black text
    ]
        |> group


viewLevelSelect : Float2 -> LevelButtons -> List Shape
viewLevelSelect mouse lbs =
    [ words black "Select Level"
        |> scale 1.5
        |> moveUp lbs.top
        |> moveUp 60
    , renderLevelButtons mouse lbs
    ]



-- Scene


type Scene
    = Intro
    | LevelSelect Int
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
    = IntroSceneClicked
    | LevelButtonClicked Int
    | PuzzleSceneBtnClicked PuzzleButton
    | UpdatePuzzleGrid
    | NoOp


toMsg : Computer2 -> Scene -> Msg
toMsg computer scene =
    case ( scene, computer.mouse.event ) of
        ( Intro, Click _ ) ->
            IntroSceneClicked

        ( LevelSelect levelCount, Click pt ) ->
            levelButtonIdxAt pt computer.screen levelCount
                |> Maybe.map LevelButtonClicked
                |> Maybe.withDefault NoOp

        ( PuzzleScene _, ev ) ->
            puzzleSceneButtons computer.screen
                |> List.filterMap (Button.onClick ev)
                |> List.head
                |> Maybe.map PuzzleSceneBtnClicked
                |> Maybe.withDefault UpdatePuzzleGrid

        _ ->
            NoOp


updateScene : Computer2 -> Msg -> Scene -> Scene
updateScene computer msg scene =
    case ( msg, scene ) of
        ( IntroSceneClicked, _ ) ->
            initialLevelSelect

        ( LevelButtonClicked idx, _ ) ->
            Levels.fromIndex idx |> initPuzzleScene

        ( PuzzleSceneBtnClicked btn, PuzzleScene model ) ->
            case btn of
                SelectLevel ->
                    initialLevelSelect

                NextLevel ->
                    initPuzzleScene (Levels.next model.levels)

                PrevLevel ->
                    initPuzzleScene (Levels.prev model.levels)

        ( UpdatePuzzleGrid, PuzzleScene model ) ->
            PuzzleScene { model | grid = PuzzleGrid.update computer model.grid }

        _ ->
            scene


viewMem : Computer2 -> Mem -> List Shape
viewMem computer mem =
    case mem.scene of
        Intro ->
            [ words black "Tap To Start" ]

        LevelSelect levelCount ->
            let
                lbs =
                    initLevelButtons computer.screen levelCount
            in
            viewLevelSelect computer.mouse.pos lbs

        PuzzleScene puzzle ->
            viewPuzzleScene computer puzzle


main =
    Game2.game2 viewMem updateMem init
