module Demos.Zombies exposing (main)

import Color exposing (Color)
import Ecosystem exposing (Change(..))
import Environment exposing (Environment)
import Force exposing (Newtons, newtons)
import Interaction exposing (Interaction)
import Length exposing (meters)
import Maybe.Extra as Maybe
import Quantity exposing (Quantity, zero)
import Set exposing (Set)
import Speed exposing (metersPerSecond)
import Vector2d exposing (Vector2d)


main : Ecosystem.Program Actor Action
main =
    Ecosystem.simulation
        { size = meters 500
        , init = init
        , updateActor = updateActor
        , paintActor = paintActor
        , classify =
            \actor ->
                case actor of
                    Living ->
                        "Living"

                    Dead ->
                        "Infected"
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
    Ecosystem.grid 6 6 (meters 10) constructor


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
            { size = meters 1
            , fill = Color.lightBlue
            , stroke = Color.darkBlue
            }

        Dead ->
            { size = meters 1
            , fill = Color.darkRed
            , stroke = Color.lightRed
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
