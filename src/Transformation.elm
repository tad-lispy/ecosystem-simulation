module Transformation exposing
    ( Transformation(..)
    , toString
    )

import Angle exposing (Angle)
import Pixels exposing (Pixels)
import Quantity exposing (Unitless)
import Vector2d exposing (Vector2d)


type Transformation coordinates
    = Identity
    | Scale (Vector2d Unitless coordinates)
    | Translate (Vector2d Pixels coordinates)
    | Rotate Angle


toString : Transformation coordinates -> String
toString transformation =
    case transformation of
        Identity ->
            ""

        Scale vector ->
            let
                { x, y } =
                    Vector2d.toUnitless vector
            in
            "scale("
                ++ (x |> String.fromFloat)
                ++ ", "
                ++ (y |> String.fromFloat)
                ++ ")"

        Translate vector ->
            let
                { x, y } =
                    Vector2d.toPixels vector
            in
            "translate("
                ++ (x |> String.fromFloat)
                ++ ", "
                ++ (y |> String.fromFloat)
                ++ ")"

        Rotate angle ->
            let
                degrees =
                    angle
                        |> Angle.inDegrees
                        |> String.fromFloat
            in
            "rotate("
                ++ degrees
                ++ ")"
