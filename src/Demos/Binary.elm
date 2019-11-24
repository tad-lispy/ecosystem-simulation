module Demos.Binary exposing (main)

import Acceleration exposing (Acceleration, MetersPerSecondSquared)
import Color exposing (Color)
import Dict
import Duration exposing (minutes)
import Ecosystem exposing (Change(..))
import Environment exposing (Environment)
import Force exposing (Newtons, newtons)
import Length exposing (meters)
import Mass exposing (kilograms)
import Maybe.Extra as Maybe
import Quantity exposing (Quantity, zero)
import Set exposing (Set)
import Speed exposing (MetersPerSecond, Speed)
import Vector2d exposing (Vector2d)


main : Ecosystem.Program Actor Action
main =
    Ecosystem.simulation
        { size = meters 500
        , init = init
        , updateActor = updateActor
        , paintActor = paintActor
        , gatherStats = gatherStats
        , statsRetention = minutes 15
        }


type Actor
    = Pretton Velocity
    | Uglon Velocity


type alias Velocity =
    Vector2d MetersPerSecond Ecosystem.Coordinates


type alias Action =
    Never


init =
    let
        constructor : Int -> Actor
        constructor index =
            if modBy 10 index < 6 then
                Pretton Vector2d.zero

            else
                Uglon Vector2d.zero
    in
    Ecosystem.grid 14 9 (meters 49) constructor


toPrecision : Int -> Quantity Float a -> Quantity Float a
toPrecision precision value =
    let
        multiplier =
            precision
                |> (^) 10
                |> toFloat
    in
    value
        |> Quantity.multiplyBy multiplier
        |> Quantity.round
        |> Quantity.toFloatQuantity
        |> Quantity.divideBy multiplier


updateActor :
    Ecosystem.Id
    -> Actor
    -> Environment Actor Action
    -> Ecosystem.ActorUpdate Actor Action
updateActor id this environment =
    let
        dragCoefficient =
            0.03

        velocityLoss velocity =
            let
                speedLoss =
                    Quantity.for duration deceleration

                deceleration =
                    Quantity.over mass drag

                drag =
                    ((Speed.inMetersPerSecond speed * dragCoefficient) ^ 2)
                        |> newtons

                speed =
                    Vector2d.length velocity

                direction =
                    Vector2d.direction velocity
            in
            direction
                |> Maybe.map (Vector2d.withLength speedLoss)
                |> Maybe.withDefault Vector2d.zero

        mass =
            kilograms 0.05

        duration =
            Environment.latency environment

        deltaVeocity : Vector2d MetersPerSecond Ecosystem.Coordinates
        deltaVeocity =
            Vector2d.direction force
                |> Maybe.map (Vector2d.withLength acceleration)
                |> Maybe.withDefault Vector2d.zero
                |> Vector2d.for duration

        acceleration : Acceleration
        acceleration =
            force
                |> Vector2d.length
                |> Quantity.over mass

        force : Vector2d Newtons Ecosystem.Coordinates
        force =
            environment
                |> Environment.actors
                |> List.map (groupInfluence environment)
                |> List.foldl Vector2d.plus Vector2d.zero
    in
    case this of
        Pretton velocity ->
            { change =
                velocity
                    |> Vector2d.plus deltaVeocity
                    |> Vector2d.minus (velocityLoss velocity)
                    |> Pretton
                    |> Changed
            , velocity = velocity
            , spawn = []
            , interactions = []
            }

        Uglon velocity ->
            { change =
                velocity
                    |> Vector2d.plus deltaVeocity
                    |> Vector2d.minus (velocityLoss velocity)
                    |> Uglon
                    |> Changed
            , velocity = velocity
            , spawn = []
            , interactions = []
            }


paintActor : Actor -> Ecosystem.Image
paintActor actor =
    case actor of
        Uglon velocity ->
            { size = meters 1
            , fill = Color.lightBlue
            , stroke = Color.darkBlue
            }

        Pretton velocity ->
            { size = meters 2
            , fill = Color.darkRed
            , stroke = Color.lightRed
            }


groupInfluence :
    Environment Actor Action
    -> Environment.Group
    -> Vector2d Newtons Ecosystem.Coordinates
groupInfluence environment group =
    let
        charge =
            group.members
                |> Set.toList
                |> List.map (\id -> Environment.actor id environment)
                |> Maybe.values
                |> List.map
                    (\member ->
                        case member of
                            Pretton _ ->
                                2

                            Uglon _ ->
                                -1
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
            (900 / (distance ^ 3))
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
        getVelocity : Actor -> Velocity
        getVelocity actor =
            case actor of
                Uglon velocity ->
                    velocity

                Pretton velocity ->
                    velocity
    in
    actors
        |> List.map getVelocity
        |> List.map Vector2d.length
        |> List.foldl Quantity.plus Quantity.zero
        |> Quantity.divideBy (List.length actors |> toFloat)
        |> Speed.inMetersPerSecond
        |> Dict.singleton "Average speed"
