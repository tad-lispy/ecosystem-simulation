module Demo exposing (main)

import AltMath.Vector2 as Vector2 exposing (Vec2, vec2)
import Color exposing (Color)
import Ecosystem
    exposing
        ( Duration
        , Group
        , Id
        , Image
        , Interaction
        , Update
        )


constants =
    { universeSize = 2000
    , minForce = -0.001
    , repulsionMagnitude = 1.5
    , repulsionScale = 15
    , attractionMagnitude = 1
    , attractionScale = 1
    }


main =
    Ecosystem.simulation
        { update = update
        , init = init
        , view = view
        , size = constants.universeSize
        }


type Entity
    = Pretton
    | Uglon


type alias Action =
    ()


init : List (Update Entity Action)
init =
    Ecosystem.grid
        12
        9
        23
        (\index ->
            if modBy 5 index < 3 then
                Uglon

            else
                Pretton
        )


update :
    Duration
    -> Id
    -> Entity
    -> List (Interaction Action)
    -> List (Group Entity)
    -> Update Entity Action
update duration id entity interactions groups =
    {- The behavior is the same so we don't need to differentiate between different kinds of entities. If we want to introduce differences, then pattern matching with

           case entity of
               Uglon ->
                   ...
               Pretton ->
                   ...

       would work
    -}
    let
        mass : Float
        mass =
            0.01

        forces =
            List.map force groups
    in
    { this = Just entity
    , interactions = []
    , movement =
        forces
            |> List.foldl Vector2.add equilibrium
            |> Vector2.scale (duration / mass)
    }


view : Entity -> Image
view entity =
    case entity of
        Uglon ->
            { fill = Color.lightYellow
            , stroke = Color.orange
            , size = 5
            }

        Pretton ->
            { fill = Color.lightBlue
            , stroke = Color.blue
            , size = 5
            }



-- Helpers


equilibrium =
    vec2 0 0


charge : Group Entity -> Float
charge group =
    group.members
        |> List.map Tuple.second
        |> List.foldl
            (\member memo ->
                case member of
                    Uglon ->
                        memo - 1

                    Pretton ->
                        memo + 1
            )
            0


force : Group Entity -> Vec2
force group =
    let
        strength =
            (attraction - repulsion)
                |> max constants.minForce

        direction =
            Vector2.normalize group.position

        attraction =
            (charge group * constants.attractionScale)
                / (distance ^ constants.attractionMagnitude)

        repulsion =
            constants.repulsionScale
                / (distance ^ constants.repulsionMagnitude)

        distance =
            Vector2.lengthSquared group.position
    in
    Vector2.scale strength direction
