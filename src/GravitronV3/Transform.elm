module GravitronV3.Transform exposing
    ( Transform
    , initial
    , renderRectTransform
    , renderTransform
    )


type Transform
    = Transform Float Float Float Float


initial : Transform
initial =
    Transform 0 0 0 1


renderRectTransform : Float -> Float -> Transform -> String
renderRectTransform w h t =
    let
        f =
            String.fromFloat
    in
    renderTransform t
        ++ (" translate(" ++ f (-w / 2) ++ "," ++ f (-h / 2) ++ ")")


andThenRotateBy : Float -> String -> String
andThenRotateBy angle prefix =
    if angle == 0 then
        prefix

    else
        prefix ++ (" rotate(" ++ String.fromFloat angle ++ ")")


renderTranslate : Float -> Float -> String
renderTranslate x y =
    "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"


andThenScaleBy : Float -> String -> String
andThenScaleBy s prefix =
    if s == 1 then
        prefix

    else
        prefix ++ (" scale(" ++ String.fromFloat s ++ ")")


renderTransform : Transform -> String
renderTransform (Transform dx dy angle s) =
    renderTranslate dx dy
        |> andThenRotateBy angle
        |> andThenScaleBy s
