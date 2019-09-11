module Cluster exposing
    ( Cluster(..)
    , clusters
    , insert
    , location
    , remove
    )

import AltMath.Vector2 as Vector2 exposing (Vec2, vec2)
import IntDict exposing (IntDict)


type Cluster
    = Empty Float
    | Singleton Float (List Entity) Vec2
    | Cluster Float (IntDict Vec2) SubClusters


type alias Entity =
    Int


type alias SubClusters =
    { topLeft : Cluster
    , topRight : Cluster
    , bottomLeft : Cluster
    , bottomRight : Cluster
    }


insert : Vec2 -> Entity -> Cluster -> Cluster
insert position entity cluster =
    case cluster of
        Empty size ->
            Singleton
                size
                [ entity ]
                position

        Singleton size existingEntities existingPoint ->
            if existingPoint == position then
                Singleton size (entity :: existingEntities) existingPoint

            else
                let
                    emptyCluster =
                        Cluster
                            size
                            IntDict.empty
                            emptySubClusters

                    emptySubClusters =
                        SubClusters
                            emptySubCluster
                            emptySubCluster
                            emptySubCluster
                            emptySubCluster

                    emptySubCluster =
                        Empty (size / 2)
                in
                existingEntities
                    |> List.foldl
                        (\existingEntity memo ->
                            insert existingPoint existingEntity memo
                        )
                        emptyCluster
                    |> insert position entity

        Cluster size entities existingSubClusters ->
            let
                subClusters =
                    case
                        ( Vector2.getX position < (size / 2)
                        , Vector2.getY position < (size / 2)
                        )
                    of
                        ( True, True ) ->
                            { existingSubClusters
                                | topLeft =
                                    existingSubClusters.topLeft
                                        |> insert position entity
                            }

                        ( False, True ) ->
                            let
                                x =
                                    Vector2.getX position - (size / 2)
                            in
                            { existingSubClusters
                                | topRight =
                                    existingSubClusters.topRight
                                        |> insert (Vector2.setX x position) entity
                            }

                        ( True, False ) ->
                            let
                                y =
                                    Vector2.getY position - (size / 2)
                            in
                            { existingSubClusters
                                | bottomLeft =
                                    existingSubClusters.bottomLeft
                                        |> insert (Vector2.setY y position) entity
                            }

                        ( False, False ) ->
                            let
                                x =
                                    Vector2.getX position - (size / 2)

                                y =
                                    Vector2.getY position - (size / 2)
                            in
                            { existingSubClusters
                                | bottomRight =
                                    existingSubClusters.bottomRight
                                        |> insert (vec2 x y) entity
                            }
            in
            Cluster
                size
                (IntDict.insert entity position entities)
                subClusters


remove : Entity -> Cluster -> Cluster
remove entity cluster =
    case cluster of
        Empty _ ->
            cluster

        Singleton size existingEntities position ->
            case List.filter ((/=) entity) existingEntities of
                [] ->
                    Empty size

                rest ->
                    Singleton size rest position

        Cluster size entities existingSubClusters ->
            case IntDict.get entity entities of
                Nothing ->
                    cluster

                Just position ->
                    let
                        remainingEntities =
                            IntDict.remove entity entities
                    in
                    case IntDict.toList remainingEntities of
                        [] ->
                            Empty size

                        ( lastEntity, lastPosition ) :: [] ->
                            -- NOTE: It is possible that more than one entity
                            -- exsits in the same position. Ideally in such
                            -- case the cluster should collapse into a
                            -- singleton. Implementing it would add complexity
                            -- and I assume it's a rare situation. So collapse
                            -- will happen only when there is only one entity
                            -- left.
                            Singleton size [ lastEntity ] lastPosition

                        _ ->
                            let
                                subClusters =
                                    case
                                        ( Vector2.getX position < (size / 2)
                                        , Vector2.getY position < (size / 2)
                                        )
                                    of
                                        ( True, True ) ->
                                            { existingSubClusters
                                                | topLeft =
                                                    existingSubClusters.topLeft
                                                        |> remove entity
                                            }

                                        ( False, True ) ->
                                            { existingSubClusters
                                                | topRight =
                                                    existingSubClusters.topRight
                                                        |> remove entity
                                            }

                                        ( True, False ) ->
                                            { existingSubClusters
                                                | bottomLeft =
                                                    existingSubClusters.bottomLeft
                                                        |> remove entity
                                            }

                                        ( False, False ) ->
                                            { existingSubClusters
                                                | bottomRight =
                                                    existingSubClusters.bottomRight
                                                        |> remove entity
                                            }
                            in
                            Cluster
                                size
                                remainingEntities
                                subClusters


location : Entity -> Cluster -> Maybe Vec2
location entity cluster =
    case cluster of
        Empty _ ->
            Nothing

        Singleton _ entities position ->
            if List.member entity entities then
                Just position

            else
                Nothing

        Cluster _ entities _ ->
            IntDict.get entity entities


clusters : Float -> Vec2 -> Cluster -> List ( Vec2, List Entity )
clusters precision viewpoint cluster =
    case cluster of
        Empty _ ->
            []

        Singleton _ entities position ->
            [ ( position, entities ) ]

        Cluster size entities subClusters ->
            let
                center =
                    vec2 (size / 2) (size / 2)

                distance =
                    Vector2.distance viewpoint center
            in
            if size / distance < precision then
                [ ( center, IntDict.keys entities ) ]

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
                    |> clusters
                        precision
                        viewpoint
                , subClusters.topRight
                    |> clusters
                        precision
                        (Vector2.sub viewpoint topRightTranslation)
                    |> List.map
                        (Tuple.mapFirst (Vector2.add topRightTranslation))
                , subClusters.bottomLeft
                    |> clusters
                        precision
                        (Vector2.sub viewpoint bottomLeftTranslation)
                    |> List.map
                        (Tuple.mapFirst (Vector2.add bottomLeftTranslation))
                , subClusters.bottomRight
                    |> clusters
                        precision
                        (Vector2.sub viewpoint bottomRightTranslation)
                    |> List.map
                        (Tuple.mapFirst (Vector2.add bottomRightTranslation))
                ]
                    |> List.concat
