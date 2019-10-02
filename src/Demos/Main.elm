module Demos.Main exposing (main)

import Angle exposing (Angle)
import Color exposing (Color)
import Direction2d exposing (Direction2d)
import Duration exposing (Duration)
import Ecosystem
    exposing
        ( ActorUpdate
        , Change(..)
        , Coordinates
        , Id
        , Image
        , Spawn
        )
import Environment
    exposing
        ( Environment
        , Group
        )
import Force exposing (Force, Newtons)
import Interaction exposing (Interaction)
import Length exposing (Length, Meters)
import Mass exposing (Mass)
import Maybe.Extra as Maybe
import Quantity exposing (Quantity, Rate, zero)
import Set exposing (Set)
import Speed exposing (MetersPerSecond, Speed)
import Vector2d exposing (Vector2d)


main : Ecosystem.Program Actor Action
main =
    Ecosystem.simulation
        { size = Length.meters 500
        , updateActor = updateActor
        , paintActor = paintActor
        , init = init
        }


type alias Actor =
    ()


type alias Action =
    ()


updateActor :
    Id
    -> Actor
    -> Environment Actor Action
    -> ActorUpdate Actor Action
updateActor id this environment =
    let
        speed =
            Quantity.per
                (Duration.seconds 1)
                (Length.meters 5)

        duration =
            Environment.latency environment

        distance =
            Quantity.for
                duration
                speed

        nearest =
            environment
                |> Environment.actors
                |> List.sortBy
                    (.position
                        >> Vector2d.length
                        >> Length.inMeters
                    )
                |> List.head

        direction =
            nearest
                |> Maybe.map .position
                |> Maybe.andThen Vector2d.direction
                |> Maybe.map Direction2d.reverse

        movement =
            direction
                |> Maybe.map (Vector2d.withLength distance)
                |> Maybe.withDefault Vector2d.zero

        spawn =
            case direction of
                Nothing ->
                    [ { actor = this
                      , displacement = Vector2d.meters 3 0
                      , interactions = []
                      }
                    , { actor = this
                      , displacement = Vector2d.meters 0 3
                      , interactions = []
                      }
                    , { actor = this
                      , displacement = Vector2d.meters -3 0
                      , interactions = []
                      }
                    , { actor = this
                      , displacement = Vector2d.meters 0 -3
                      , interactions = []
                      }
                    ]

                Just away ->
                    if
                        nearest
                            |> Maybe.map .position
                            |> Maybe.map Vector2d.length
                            |> Maybe.withDefault zero
                            |> Quantity.greaterThan (Length.meters 20)
                    then
                        [ { actor = this
                          , displacement =
                                Vector2d.withLength
                                    (Length.meters 2)
                                    away
                          , interactions = []
                          }
                        ]

                    else
                        []
    in
    { change = Unchanged
    , movement = movement
    , interactions = []
    , spawn = spawn
    }


paintActor : actor -> Image
paintActor actor =
    { size = Length.meters 1
    , fill = Color.white
    , stroke = Color.green
    }


init =
    let
        constructor : Int -> ()
        constructor id =
            ()
    in
    Ecosystem.grid
        1
        1
        (Length.meters 10)
        constructor
