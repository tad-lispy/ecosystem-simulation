module WrappedPlane exposing
    ( Group
    , Id
    , Plane
    , anchor
    , clusters
    , empty
    , foldl
    , place
    , remove
    , render
    , return
    , shift
    , shiftTo
    )

import AltMath.Vector2 as Vector2 exposing (Vec2, vec2)
import Basics.Extra exposing (fractionalModBy)
import Cluster exposing (Cluster(..))
import IntDict exposing (IntDict)
import Maybe.Extra as Maybe
import Set


type Plane entity
    = Plane
        { cluster : Cluster
        , entities : IntDict entity
        , size : Float
        , cursor : Vec2
        , anchor : Vec2
        }


type alias Id =
    Int


type alias Group entity =
    { members : List ( Id, entity )
    , position : Vec2
    }


empty : Float -> Plane entity
empty size =
    Plane
        { cluster = Empty size
        , entities = IntDict.empty
        , cursor = vec2 0 0
        , anchor = vec2 0 0
        , size = size
        }


anchor : Plane entity -> Plane entity
anchor (Plane this) =
    Plane { this | anchor = this.cursor }


return : Plane entity -> Plane entity
return (Plane this) =
    Plane { this | cursor = this.anchor }


place :
    Id
    -> entity
    -> Plane entity
    -> Plane entity
place id entity (Plane this) =
    Plane
        { this
            | cluster =
                this.cluster
                    |> Cluster.remove id
                    |> Cluster.insert this.cursor id
            , entities =
                this.entities
                    |> IntDict.insert id entity
        }


remove : Id -> Plane entity -> Plane entity
remove id (Plane this) =
    Plane
        { this
            | cluster =
                Cluster.remove id this.cluster
        }


foldl : (Id -> entity -> a -> a) -> a -> Plane entity -> a
foldl reducer initial (Plane this) =
    this.entities
        |> IntDict.foldl reducer initial


render :
    Float
    -> Float
    -> Plane entity
    -> List ( Id, entity, Vec2 )
render width height (Plane this) =
    case this.cluster of
        Cluster.Empty _ ->
            []

        Cluster.Singleton _ entities location ->
            entities
                |> Set.toList
                |> List.map
                    (\id ->
                        case IntDict.get id this.entities of
                            Nothing ->
                                Nothing

                            Just entity ->
                                Just
                                    ( id
                                    , entity
                                    , from this.size this.cursor location
                                    )
                    )
                |> Maybe.values

        Cluster _ entities _ ->
            entities
                |> IntDict.toList
                |> List.map
                    (\( id, location ) ->
                        case IntDict.get id this.entities of
                            Nothing ->
                                Nothing

                            Just entity ->
                                Just
                                    ( id
                                    , entity
                                    , from this.size this.cursor location
                                    )
                    )
                |> Maybe.values


shift : Vec2 -> Plane entity -> Plane entity
shift vector (Plane this) =
    Plane
        { this
            | cursor =
                this.cursor
                    |> Vector2.add vector
                    |> wrap this.size
        }


shiftTo : Id -> Plane entity -> Maybe (Plane entity)
shiftTo id (Plane this) =
    this.cluster
        |> Cluster.location id
        |> Maybe.map
            (\point -> { this | cursor = point })
        |> Maybe.map Plane


{-| Place the point in a coordinate system between -size / 2 and size / 2

    Point2d.fromCoordinates (0, 0)
        |> wrap 100
        --> Point2d.fromCoordinates (0, 0)

    Point2d.fromCoordinates (20, 20)
        |> wrap 100
        --> Point2d.fromCoordinates (20, 20)

    Point2d.fromCoordinates (60, 20)
        |> wrap 100
        --> Point2d.fromCoordinates (-40, 20)

-}
wrap : Float -> Vec2 -> Vec2
wrap size point =
    vec2
        (fractionalModBy size (Vector2.getX point))
        (fractionalModBy size (Vector2.getY point))


clusters :
    Float
    -> Plane entity
    -> List (Group entity)
clusters precision (Plane universe) =
    let
        internal : Vec2 -> Cluster -> List (Group entity)
        internal origin cluster =
            case cluster of
                Empty _ ->
                    []

                Singleton _ ids location ->
                    [ { position = from universe.size origin location
                      , members =
                            universe.entities
                                |> IntDict.filter
                                    (\id _ ->
                                        Set.member id ids
                                    )
                                |> IntDict.toList
                      }
                    ]

                Cluster size locations subClusters ->
                    let
                        center =
                            vec2 (size / 2) (size / 2)

                        position =
                            from universe.size origin center

                        distance =
                            Vector2.length position

                        ids =
                            locations
                                |> IntDict.keys
                                |> Set.fromList
                    in
                    if size / distance < precision then
                        [ { position = position
                          , members =
                                universe.entities
                                    |> IntDict.filter
                                        (\id _ ->
                                            Set.member id ids
                                        )
                                    |> IntDict.toList
                          }
                        ]

                    else
                        let
                            topRightTranslation =
                                vec2 (size / 2) 0

                            bottomLeftTranslation =
                                vec2 0 (size / 2)

                            bottomRightTranslation =
                                vec2 (size / 2) (size / 2)
                        in
                        [ subClusters.topLeft
                            |> internal origin
                        , subClusters.topRight
                            |> internal
                                (Vector2.sub origin topRightTranslation)
                        , subClusters.bottomLeft
                            |> internal
                                (Vector2.sub origin bottomLeftTranslation)
                        , subClusters.bottomRight
                            |> internal
                                (Vector2.sub origin bottomRightTranslation)
                        ]
                            |> List.concat
    in
    internal universe.cursor universe.cluster


from : Float -> Vec2 -> Vec2 -> Vec2
from size start end =
    let
        halfsize =
            vec2 (size / 2) (size / 2)
    in
    end
        |> Vector2.add halfsize
        |> Vector2.sub start
        |> wrap size
        |> Vector2.sub halfsize

