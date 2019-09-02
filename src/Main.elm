module Main exposing (main)

import Browser
import Browser.Events
import Circle2d
import Geometry.Svg
import Html exposing (Html)
import Html.Events
import List.Extra as List
import Maybe.Extra as Maybe
import Point2d exposing (Point2d)
import Result.Extra as Result
import Surface exposing (Surface)
import Svg exposing (Svg)
import Svg.Attributes
import Vector2d


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    ()


type alias Model =
    { surface : Surface
    , entities : List Int
    , paused : Bool
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        entities =
            List.range 0 30
    in
    ( { entities = entities
      , surface =
            entities
                |> List.foldl
                    (\id surface ->
                        surface
                            |> Surface.shift
                                (Vector2d.fromPolarComponents
                                    ( 30, toFloat id |> degrees )
                                )
                            |> Surface.place id
                    )
                    (Surface.empty 500)
      , paused = False
      }
    , Cmd.none
    )


type Msg
    = Animate Float
    | Insert
    | Play
    | Pause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate delta ->
            let
                forces =
                    model.entities
                        |> List.map
                            (\id ->
                                model.surface
                                    |> Surface.shiftTo id
                                    |> Maybe.map (Surface.entities 20)
                                    |> Maybe.map (List.filter (Tuple.first >> (/=) id))
                                    |> Maybe.map (Tuple.pair id)
                            )
                        |> Maybe.values
                        |> List.map
                            (Tuple.mapSecond
                                (\neighbors ->
                                    neighbors
                                        |> List.map Tuple.second
                                        |> List.map
                                            (\vector ->
                                                vector
                                                    |> Vector2d.polarComponents
                                                    |> Tuple.mapFirst (\length -> 10 / (length ^ 2))
                                                    |> Tuple.mapFirst
                                                        (\length ->
                                                            if isNaN length || isInfinite length then
                                                                0

                                                            else
                                                                length
                                                        )
                                                    |> Vector2d.fromPolarComponents
                                                    |> Vector2d.reverse
                                            )
                                        |> List.foldl1 Vector2d.sum
                                        |> Maybe.withDefault Vector2d.zero
                                )
                            )

                updateSurface surface =
                    forces
                        |> List.foldl
                            (\( id, force ) memo ->
                                memo
                                    |> Surface.shiftTo id
                                    |> Result.fromMaybe ("Shifting to non existent entity " ++ String.fromInt id)
                                    |> Result.withDefault memo
                                    |> Surface.shift (Vector2d.scaleBy virtualDelta force)
                                    -- |> Surface.shift (Vector2d.fromComponents ( 1, 2 ))
                                    |> Surface.place id
                            )
                            surface

                virtualDelta =
                    min delta 32

                shift =
                    Vector2d.fromComponents ( virtualDelta / 100, 0 )
            in
            ( { model
                | surface =
                    model.surface
                        --                        |> Surface.shift shift
                        |> Surface.place -1
                        |> updateSurface
                        |> Surface.shiftTo -1
                        |> Result.fromMaybe "We lost our anchor!"
                        |> Result.withDefault model.surface
                        |> Surface.remove -1
              }
            , Cmd.none
            )

        Insert ->
            let
                next =
                    List.length model.entities
            in
            ( { model
                | entities = next :: model.entities
                , surface =
                    Surface.place next model.surface
              }
            , Cmd.none
            )

        Play ->
            ( { model | paused = False }
            , Cmd.none
            )

        Pause ->
            ( { model | paused = True }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused then
        Sub.none

    else
        Browser.Events.onAnimationFrameDelta Animate



-- Sub.none


view : Model -> Html Msg
view model =
    Html.div []
        [ model.surface
            |> Surface.render 500 500
            |> scene
        , if model.paused then
            Html.button [ Html.Events.onClick Play ] [ Html.text "Play" ]

          else
            Html.button [ Html.Events.onClick Pause ] [ Html.text "Pause" ]
        , Html.button [ Html.Events.onClick (Animate 16) ] [ Html.text "Step" ]
        , Html.button [ Html.Events.onClick Insert ] [ Html.text "Insert" ]
        , Html.text <| String.fromInt <| List.length <| model.entities
        ]


scene : List ( Int, Point2d ) -> Svg msg
scene entities =
    entities
        |> List.map Tuple.second
        |> List.map
            (Circle2d.withRadius 3)
        |> List.map
            (Geometry.Svg.circle2d [ Svg.Attributes.fill "red" ])
        |> Svg.svg
            [ Svg.Attributes.width "500"
            , Svg.Attributes.height "500"
            , Svg.Attributes.style "background: pink"
            , Svg.Attributes.viewBox "-250 -250 500 500"
            ]
