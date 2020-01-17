module MirrorPuzzleV2.Main exposing (main)

import List.Extra
import MirrorPuzzleV2.Box as Box exposing (Box)
import MirrorPuzzleV2.PuzzleGrid as PuzzleGrid exposing (PuzzleGrid)
import Playground exposing (..)
import Playground.Extra exposing (..)



-- Scenes: Puzzle Model


type alias PuzzleSceneModel =
    { grid : PuzzleGrid
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
    , renderButton mouse "Select Level" (initBackButtonBox screen)
    , if isSolved then
        renderButton mouse "Next" (initNextButtonBox screen)

      else
        noShape
    ]


caseBool : Bool -> c -> c -> c
caseBool bool true false =
    if bool then
        true

    else
        false


initBackButtonBox : Screen -> Box
initBackButtonBox screen =
    Box.atTopLeft 150 30
        |> Box.move ( screen.left, screen.top )


initNextButtonBox : Screen -> Box
initNextButtonBox screen =
    Box.atOrigin 100 30
        |> Box.move ( screen.right, screen.top )
        |> Box.moveDown 50
        |> Box.moveLeft 100



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
                    if mouse.click then
                        if Box.containsXY mouse (initBackButtonBox screen) then
                            initialLevelSelect

                        else
                            PuzzleScene { model | grid = PuzzleGrid.update computer model.grid }

                    else
                        mem.scene
            in
            { mem | scene = nextScene }


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
