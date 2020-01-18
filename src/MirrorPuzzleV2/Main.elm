module MirrorPuzzleV2.Main exposing (main)

import List.Extra
import MirrorPuzzleV2.Box as Box exposing (Box)
import MirrorPuzzleV2.PuzzleGrid as PuzzleGrid
import Playground exposing (..)
import Playground.Extra exposing (..)



-- Scenes: Puzzle Model


type alias PuzzleSceneModel =
    { grid : PuzzleGrid.Model
    , levelIdx : Int
    }


initialPuzzleScene_ : Scene
initialPuzzleScene_ =
    PuzzleScene { grid = PuzzleGrid.initial, levelIdx = 0 }


initPuzzleScene : Int -> Scene
initPuzzleScene levelIdx =
    case List.drop levelIdx PuzzleGrid.levels |> List.head of
        Just levelStr ->
            PuzzleScene { grid = PuzzleGrid.fromString levelStr, levelIdx = levelIdx }

        Nothing ->
            initialPuzzleScene_


goToLevelBy : Int -> PuzzleSceneModel -> Scene
goToLevelBy offset model =
    clamp 0 ((PuzzleGrid.levels |> List.length) - 1) (model.levelIdx + offset)
        |> initPuzzleScene


viewPuzzleScene : Computer -> PuzzleSceneModel -> List Shape
viewPuzzleScene computer { grid, levelIdx } =
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
         , String.fromInt (levelIdx + 1)
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


puzzleSceneButtons =
    [ SelectLevel, NextLevel, PrevLevel ]


puzzleSceneBtnAt : Mouse -> Screen -> Maybe PuzzleButton
puzzleSceneBtnAt mouse screen =
    puzzleSceneButtons
        |> List.map (puzzleBtnData screen)
        |> List.Extra.findIndex (Tuple.second >> Box.containsXY mouse)
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


viewPuzzleSceneButton : Mouse -> Screen -> PuzzleButton -> Shape
viewPuzzleSceneButton mouse screen btn =
    let
        ( text, box ) =
            puzzleBtnData screen btn
    in
    renderButton mouse text box


viewPuzzleSceneButtons : Mouse -> Screen -> Shape
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
    LevelSelect (PuzzleGrid.levels |> List.length)


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


levelButtonIdxFromMouse : Mouse -> LevelButtons -> Maybe Int
levelButtonIdxFromMouse mouse lbs =
    lbs.list
        |> List.Extra.findIndex (Box.containsXY mouse)


renderLevelButtons : Mouse -> LevelButtons -> Shape
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


renderButton : Mouse -> String -> Box -> Shape
renderButton mouse text rect =
    buttonShape (Box.containsXY mouse rect)
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


viewLevelSelect : Mouse -> LevelButtons -> List Shape
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
    { scene = initPuzzleScene 0 }


updateMem : Computer -> Mem -> Mem
updateMem computer mem =
    let
        { mouse, screen } =
            computer
    in
    case mem.scene of
        Intro ->
            if mouse.click then
                { mem | scene = initialLevelSelect }

            else
                mem

        LevelSelect levelCount ->
            let
                lbs =
                    initLevelButtons screen levelCount
            in
            case ( mouse.click, levelButtonIdxFromMouse mouse lbs ) of
                ( True, Just i ) ->
                    { mem | scene = initPuzzleScene i }

                _ ->
                    mem

        PuzzleScene model ->
            let
                nextScene =
                    case ( mouse.click, puzzleSceneBtnAt mouse screen ) of
                        ( True, Just btn ) ->
                            case btn of
                                SelectLevel ->
                                    initialLevelSelect

                                NextLevel ->
                                    goToLevelBy 1 model

                                PrevLevel ->
                                    goToLevelBy -1 model

                        _ ->
                            PuzzleScene { model | grid = PuzzleGrid.update computer model.grid }
            in
            { mem | scene = nextScene }


updatePuzzleScene : Computer -> PuzzleSceneModel -> Scene
updatePuzzleScene ({ mouse, screen } as computer) model =
    if mouse.click then
        case puzzleSceneBtnAt mouse screen of
            Just btn ->
                case btn of
                    SelectLevel ->
                        initialLevelSelect

                    NextLevel ->
                        goToLevelBy 1 model

                    PrevLevel ->
                        goToLevelBy -1 model

            Nothing ->
                PuzzleScene { model | grid = PuzzleGrid.update computer model.grid }

    else
        PuzzleScene model


view : Computer -> Mem -> List Shape
view computer mem =
    case mem.scene of
        Intro ->
            [ words black "Tap To Start" ]

        LevelSelect levelCount ->
            let
                lbs =
                    initLevelButtons computer.screen levelCount
            in
            viewLevelSelect computer.mouse lbs

        PuzzleScene puzzle ->
            viewPuzzleScene computer puzzle


main =
    game view updateMem init
