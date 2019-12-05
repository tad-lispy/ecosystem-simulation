module Demos.Empty exposing (main)

import Color exposing (Color)
import Dict
import Ecosystem exposing (ActorUpdate, Change(..), Id, Spawn)
import Environment exposing (Environment)
import Interaction exposing (Interaction)
import Length exposing (Length, meters)
import Quantity exposing (zero)
import Speed exposing (metersPerSecond)
import Vector2d exposing (Vector2d)


main =
    Ecosystem.simulation
        { size = meters 500
        , updateActor = updateActor
        , paintActor = paintActor
        , paintBackground = always Color.black
        , init = init
        , gatherStats = always (Dict.singleton "Void" 0)
        , statsRetention = zero
        }


init =
    []


updateActor id this environment =
    { velocity = Vector2d.zero
    , change = Unchanged
    , spawn = []
    , interactions = []
    }


paintActor actor =
    Ecosystem.Dot
        { size = meters 0.2
        , fill = Color.lightBlue
        , stroke = Color.blue
        }
