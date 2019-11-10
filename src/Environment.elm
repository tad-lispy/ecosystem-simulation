module Environment exposing
    ( Environment
    , Group
    , actor
    , actors
    , create
    , interactions
    , latency
    )

import Duration exposing (Duration)
import IntDict exposing (IntDict)
import Interaction exposing (Interaction)
import Set exposing (Set)
import Vector2d exposing (Vector2d)
import WrappedPlane exposing (Plane)


type Environment actor action
    = Environment (Snapshot actor action)


type alias Id =
    Int


type alias Snapshot actor action =
    { actors : IntDict actor
    , surface : Plane
    , latency : Duration
    , interactions : List (Interaction action)
    }


type alias Group =
    -- Alias and re-expose
    WrappedPlane.Group


create :
    IntDict actor
    -> Plane
    -> Duration
    -> List (Interaction action)
    -> Environment actor action
create actors_ surface latency_ interactions_ =
    Environment <|
        { actors = actors_
        , surface = surface
        , interactions = interactions_
        , latency = latency_
        }


actors :
    Environment actor action
    -> List Group
actors (Environment environment) =
    environment.surface
        |> WrappedPlane.clusters 0.99


actor : Id -> Environment actor action -> Maybe actor
actor id (Environment environment) =
    environment.actors
        |> IntDict.get id


latency : Environment actor action -> Duration
latency (Environment environment) =
    environment.latency


interactions :
    Environment actor action
    -> List (Interaction action)
interactions (Environment environment) =
    environment.interactions
