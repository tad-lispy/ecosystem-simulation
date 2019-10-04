module Main exposing (main)

import Color exposing (Color)
import Direction2d
import Ecosystem exposing (ActorUpdate, Change(..), Id, Spawn)
import Environment exposing (Environment)
import Interaction exposing (Interaction)
import Length exposing (Length, meters)
import Maybe.Extra as Maybe
import Quantity
import Speed
import Vector2d exposing (Vector2d)


main =
    Ecosystem.simulation
        { size = meters 500
        , updateActor = updateActor
        , paintActor = paintActor
        , init = init
        }


type alias Actor =
    ()


type alias Action =
    ()


init =
    [ { actor = ()
      , displacement = Vector2d.zero
      , interactions = []
      }
    , { actor = ()
      , displacement = Vector2d.meters 0 -5
      , interactions = []
      }
    ]


updateActor id this environment =
    let
        speed =
            Speed.metersPerSecond 5

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

        velocity =
            direction
                |> Maybe.map (Vector2d.withLength speed)
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
                            |> Maybe.withDefault Vector2d.zero
                            |> Vector2d.length
                            |> Quantity.greaterThan (Length.meters 50)
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
    { velocity = velocity
    , change = Unchanged
    , spawn = spawn
    , interactions = []
    }


paintActor actor =
    { size = Length.meters 1
    , fill = Color.white
    , stroke = Color.green
    }

