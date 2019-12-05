module Demos.Zombies exposing (main)

import Color exposing (Color)
import Dict
import Duration exposing (Duration, minutes)
import Ecosystem exposing (Change(..))
import Environment exposing (Environment)
import Force exposing (Newtons, newtons)
import Interaction exposing (Interaction)
import Length exposing (meters)
import List.Extra as List
import Maybe.Extra as Maybe
import Quantity exposing (Quantity, zero)
import Set exposing (Set)
import Speed exposing (metersPerSecond)
import Vector2d exposing (Vector2d)


main : Ecosystem.Program Actor Action
main =
    Ecosystem.simulation
        { size = meters 300
        , init = init
        , updateActor = updateActor
        , paintActor = paintActor
        , paintBackground = paintBackground
        , gatherStats = gatherStats
        , statsRetention = minutes 15
        }


type Actor
    = Living
    | Dead


type Action
    = Bite


init =
    let
        constructor : Int -> Actor
        constructor index =
            if modBy 10 index < 8 then
                Living

            else
                Dead
    in
    Ecosystem.grid 8 12 (meters 15) constructor


updateActor :
    Ecosystem.Id
    -> Actor
    -> Environment Actor Action
    -> Ecosystem.ActorUpdate Actor Action
updateActor id this environment =
    case this of
        Living ->
            let
                speed =
                    metersPerSecond 15

                gotBitten =
                    environment
                        |> Environment.interactions
                        |> List.head
                        |> Maybe.map .action
                        |> Maybe.map ((==) Bite)
                        |> Maybe.withDefault False

                _ =
                    environment
                        |> Environment.interactions
                        |> List.map (\i -> { zombie = i.other, victim = id })
            in
            if gotBitten then
                { change =
                    Changed Dead
                , velocity =
                    Vector2d.zero
                , spawn = []
                , interactions = []
                }

            else
                { change =
                    Unchanged
                , velocity =
                    environment
                        |> Environment.actors
                        |> List.map (groupInfluence this environment)
                        |> List.foldl Vector2d.plus Vector2d.zero
                        |> Vector2d.normalize
                        |> Vector2d.direction
                        |> Maybe.map (Vector2d.withLength speed)
                        |> Maybe.withDefault Vector2d.zero
                , spawn = []
                , interactions = []
                }

        Dead ->
            let
                speed =
                    metersPerSecond 15

                victim : Maybe Ecosystem.Id
                victim =
                    environment
                        |> Environment.actors
                        |> List.filter (\group -> group.position |> Vector2d.length |> Quantity.lessThan (meters 2))
                        |> List.map
                            (\group ->
                                group.members
                                    |> Set.toList
                                    |> List.filter (\actorId -> Environment.actor actorId environment == Just Living)
                            )
                        |> List.concat
                        |> List.head

                _ =
                    victim
                        |> Maybe.map
                            (\v -> { victim = v, zombie = id })
            in
            { change =
                Unchanged
            , velocity =
                environment
                    |> Environment.actors
                    |> List.map (groupInfluence this environment)
                    |> List.foldl Vector2d.plus Vector2d.zero
                    |> Vector2d.normalize
                    |> Vector2d.direction
                    |> Maybe.map (Vector2d.withLength speed)
                    |> Maybe.withDefault Vector2d.zero
            , spawn = []
            , interactions =
                victim
                    |> Maybe.map (Interaction Bite)
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault []
            }


paintActor : Actor -> Ecosystem.Image
paintActor actor =
    case actor of
        Living ->
            Ecosystem.Text
                { size = meters 1
                , content = "👨\u{200D}💼"
                }

        Dead ->
            Ecosystem.Text
                { size = meters 1
                , content = "\u{1F9DF}\u{200D}" -- Zombie
                }


groupInfluence :
    Actor
    -> Environment Actor Action
    -> Environment.Group
    -> Vector2d Newtons Ecosystem.Coordinates
groupInfluence this environment group =
    let
        charge =
            group.members
                |> Set.toList
                |> List.map (\id -> Environment.actor id environment)
                |> Maybe.values
                |> List.map
                    (\member ->
                        case ( this, member ) of
                            ( Living, Living ) ->
                                1

                            ( Living, Dead ) ->
                                -100

                            ( Dead, Living ) ->
                                3

                            ( Dead, Dead ) ->
                                0
                    )
                |> List.foldl (+) 0

        distance =
            group.position
                |> Vector2d.length
                |> Length.inMeters

        attraction =
            (charge * 100 / (distance ^ 2))
                |> newtons

        repulsion =
            (500 / (distance ^ 3))
                |> newtons

        force =
            Quantity.minus repulsion attraction
                |> Quantity.max (newtons -100)
                |> Quantity.min (newtons 100)
    in
    group.position
        |> Vector2d.direction
        |> Maybe.map (Vector2d.withLength force)
        |> Maybe.withDefault Vector2d.zero


gatherStats actors =
    let
        countGroups =
            Tuple.mapSecond
                (List.length
                    >> (+) 1
                    >> toFloat
                )

        classify actor =
            case actor of
                Living ->
                    "Living"

                Dead ->
                    "Infected"
    in
    actors
        |> List.map classify
        |> List.gatherEquals
        |> List.map countGroups
        |> Dict.fromList


paintBackground time =
    let
        -- It's get a little bit red at night and blue at noon
        hue =
            0.8 - (0.2 * solarAltitude)

        -- Gray at noon, vivid at night
        saturation =
            1 - (0.6 * solarAltitude)

        -- Lighter at noon, darker at night
        lightness =
            0.3 * solarAltitude

        -- Solar altitude is a function of the phase of the day
        solarAltitude =
            phase
                - 0.5
                |> abs
                |> (-) 0.5
                |> (*) 2

        -- What phase of the day is it between 0 (00:00) and 1 (24:00)
        phase : Float
        phase =
            time
                |> Duration.inMilliseconds
                |> floor
                |> modBy (cycle |> Duration.inMilliseconds |> floor)
                |> toFloat
                |> (\value -> value / (cycle |> Duration.inMilliseconds))

        -- How long is the simulated day
        cycle : Duration
        cycle =
            minutes 1
    in
    Color.hsl
        hue
        saturation
        lightness
