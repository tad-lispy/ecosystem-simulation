module Demos.FPAms exposing (main)

{-| A demo simulation implemented during the FPAms meetup

With changes to make it compatible with new version of the framework

-}

import Color exposing (Color)
import Direction2d exposing (Direction2d)
import Ecosystem exposing (..)
import Environment exposing (Environment)
import Length exposing (meters)
import Vector2d


main : Ecosystem.Program Actor Action
main =
    Ecosystem.simulation
        { init = init
        , updateActor = updateActor
        , paintActor = paintActor
        , size = meters 500
        }


type alias Actor =
    ()


type alias Action =
    ()


init : List (Spawn Actor Action)
init =
    [ { actor = ()
      , displacement = Vector2d.zero
      , interactions = []
      }
    , { actor = ()
      , displacement = Vector2d.meters 10 0
      , interactions = []
      }
    ]


updateActor :
    Id
    -> Actor
    -> Environment Actor Action
    -> ActorUpdate Actor Action
updateActor id this environment =
    let
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

        movement =
            case direction of
                Nothing ->
                    Vector2d.zero

                Just d ->
                    Vector2d.withLength
                        (meters 0.5)
                        (Direction2d.reverse d)
    in
    { change = Unchanged
    , movement = movement
    , spawn = []
    , interactions = []
    }


paintActor : Actor -> Ecosystem.Image
paintActor actor =
    { size = meters 1
    , fill = Color.green
    , stroke = Color.white
    }
