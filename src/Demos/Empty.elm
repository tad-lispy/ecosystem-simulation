module Demos.Empty exposing (main)

import Color exposing (Color)
import Ecosystem exposing (ActorUpdate, Change(..), Id, Spawn)
import Environment exposing (Environment)
import Interaction exposing (Interaction)
import Length exposing (Length, meters)
import Vector2d exposing (Vector2d)


main =
    Ecosystem.simulation
        { size = meters 500
        , updateActor = updateActor
        , paintActor = paintActor
        , init = init
        }


init =
    []


updateActor id this environment =
    { movement = Vector2d.zero
    , change = Unchanged
    , spawn = []
    , interactions = []
    }


paintActor actor =
    { size = meters 0.2
    , fill = Color.lightBlue
    , stroke = Color.blue
    }
