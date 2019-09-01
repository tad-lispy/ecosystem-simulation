module Main exposing (main)

import Circle2d exposing (Circle2d)
import Geometry.Svg
import Html exposing (Html)
import Point2d exposing (Point2d)
import Surface exposing (Surface)
import Svg exposing (Svg)
import Svg.Attributes
import Vector2d exposing (Vector2d)


main =
    1000
        |> Surface.empty
        -- Focus
        |> Surface.place -1
        |> Surface.shift (Vector2d.fromComponents ( 20, 20 ))
        |> Surface.place 1
        |> Surface.shift (Vector2d.fromComponents ( 100, 20 ))
        |> Surface.place 2
        |> Surface.shiftTo -1
        -- |> Maybe.map (Surface.remove -1)
        |> Maybe.map (Surface.render 400 400)
        |> Maybe.map scene
        |> Maybe.withDefault (Html.text "Focus lost")


scene : List ( Int, Point2d ) -> Svg msg
scene entities =
    entities
        |> Debug.log "Entities"
        |> List.map Tuple.second
        |> List.map
            (Circle2d.withRadius 3)
        |> List.map
            (Geometry.Svg.circle2d [ Svg.Attributes.fill "red" ])
        |> Svg.svg
            [ Svg.Attributes.width "400"
            , Svg.Attributes.height "400"
            ]
