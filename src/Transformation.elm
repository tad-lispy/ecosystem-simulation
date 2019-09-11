module Transformation exposing
    ( Transformation(..)
    , toString
    )

import AltMath.Vector2 as Vector2 exposing (Vec2)


type Transformation
    = Identity
    | Scale Vec2
    | Translate Vec2
    | Rotate Float


toString : Transformation -> String
toString transformation =
    case transformation of
        Identity ->
            ""

        Scale vector ->
            "scale("
                ++ (Vector2.getX vector |> String.fromFloat)
                ++ ", "
                ++ (Vector2.getY vector |> String.fromFloat)
                ++ ")"

        Translate vector ->
            "translate("
                ++ (Vector2.getX vector |> String.fromFloat)
                ++ ", "
                ++ (Vector2.getY vector |> String.fromFloat)
                ++ ")"

        Rotate angle ->
            "rotate("
                ++ String.fromFloat angle
                ++ ")"
