module MirrorPuzzleV2.Main exposing (main)

import List.Extra
import Maybe.Extra
import MirrorPuzzleV2.Box as Box exposing (Box)
import MirrorPuzzleV2.Computer2 exposing (Computer2)
import MirrorPuzzleV2.Game2 as Game2
import MirrorPuzzleV2.Levels as Levels exposing (Levels)
import MirrorPuzzleV2.MouseEvent exposing (MouseEvent(..))
import MirrorPuzzleV2.PuzzleGrid as PuzzleGrid
import Number2 exposing (Float2)
import Playground exposing (..)
import Playground.Extra exposing (..)
import PointFree exposing (ignoreNothing)



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
    , viewPuzzleSceneButtons mouse.pos screen
    ]


caseBool : Bool -> c -> c -> c
caseBool bool true false =
    if bool then
        true

    else
        false


puzzleSceneButtons =
    [ SelectLevel, NextLevel, PrevLevel ]


puzzleSceneBtnAt : Float2 -> Screen -> Maybe PuzzleButton
puzzleSceneBtnAt mouse screen =
    puzzleSceneButtons
        |> List.map (puzzleBtnData screen)
        |> List.Extra.findIndex (Tuple.second >> Box.contains mouse)
        |> Maybe.andThen (\idx -> List.Extra.getAt idx puzzleSceneButtons)


type PuzzleButton
    = SelectLevel
    | NextLevel
    | PrevLevel


puzzleBtnData : Screen -> PuzzleButton -> ( String, Box )
puzzleBtnData screen puzzleButton =
    case puzzleButton of
        SelectLevel ->
            ( "Select Level", initBackButtonBox screen )

        NextLevel ->
            ( "Next", initNextButtonBox screen )

        PrevLevel ->
            ( "Prev", initPrevButtonBox screen )


viewPuzzleSceneButton : Float2 -> Screen -> PuzzleButton -> Shape
viewPuzzleSceneButton mouse screen btn =
    let
        ( text, box ) =
            puzzleBtnData screen btn
    in
    renderButton mouse text box


viewPuzzleSceneButtons : Float2 -> Screen -> Shape
viewPuzzleSceneButtons mouse screen =
    puzzleSceneButtons
        |> List.map (viewPuzzleSceneButton mouse screen)
        |> group


initBackButtonBox : Screen -> Box
initBackButtonBox screen =
    Box.atTopLeft 150 40
        |> Box.move ( screen.left, screen.top )
        |> Box.moveRight 20
        |> Box.moveDown 20


initNextButtonBox : Screen -> Box
initNextButtonBox screen =
    Box.atTopRight 100 40
        |> Box.move ( screen.right, screen.top )
        |> Box.moveDown 20
        |> Box.moveLeft 20


initPrevButtonBox : Screen -> Box
initPrevButtonBox screen =
    Box.atTopRight 100 40
        |> Box.move ( screen.right, screen.top )
        |> Box.moveDown 20
        |> Box.moveLeft (100 + 20 + 20)



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
    { mem | scene = updateScene computer mem.scene }


updateScene : Computer2 -> Scene -> Scene
updateScene computer scene =
    let
        { mouse, screen } =
            computer
    in
    case scene of
        Intro ->
            if mouse.click then
                initialLevelSelect

            else
                scene

        LevelSelect levelCount ->
            case mouse.event of
                Click p ->
                    case levelButtonIdxAt p screen levelCount of
                        Just i ->
                            initPuzzleScene (Levels.fromIndex i)

                        Nothing ->
                            scene

                _ ->
                    scene

        PuzzleScene model ->
            updatePuzzleScene computer model


updatePuzzleScene : Computer2 -> PuzzleSceneModel -> Scene
updatePuzzleScene computer model =
    case computer.mouse.event of
        Click p ->
            case puzzleSceneBtnAt p computer.screen of
                Just btn ->
                    case btn of
                        SelectLevel ->
                            initialLevelSelect

                        NextLevel ->
                            initPuzzleScene (Levels.next model.levels)

                        PrevLevel ->
                            initPuzzleScene (Levels.prev model.levels)

                Nothing ->
                    PuzzleScene { model | grid = PuzzleGrid.update computer model.grid }

        _ ->
            PuzzleScene model


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
