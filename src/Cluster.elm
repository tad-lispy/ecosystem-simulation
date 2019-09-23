module Cluster exposing
    ( Cluster(..)
    , clusters
    , insert
    , location
    , remove
    )

import AltMath.Vector2 as Vector2 exposing (Vec2, vec2)
import IntDict exposing (IntDict)
import Set exposing (Set)


type Cluster
    = Empty Float
    | Singleton Float (Set Id) Vec2
    | Cluster Float (IntDict Vec2) SubClusters


type alias Id =
    Int


type alias SubClusters =
    { topLeft : Cluster
    , topRight : Cluster
    , bottomLeft : Cluster
    , bottomRight : Cluster
    }


insert : Vec2 -> Id -> Cluster -> Cluster
insert position id cluster =
    case cluster of
        Empty size ->
            Singleton
                size
                (Set.singleton id)
                position

        Singleton size existingEntities existingPoint ->
            if existingPoint == position then
                Singleton
                    size
                    (Set.insert id existingEntities)
                    existingPoint

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
                    |> Set.foldl
                        (\existingEntity memo ->
                            insert existingPoint existingEntity memo
                        )
                        emptyCluster
                    |> insert position id

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
                                        |> insert position id
                            }

                        ( False, True ) ->
                            let
                                x =
                                    Vector2.getX position - (size / 2)
                            in
                            { existingSubClusters
                                | topRight =
                                    existingSubClusters.topRight
                                        |> insert (Vector2.setX x position) id
                            }

                        ( True, False ) ->
                            let
                                y =
                                    Vector2.getY position - (size / 2)
                            in
                            { existingSubClusters
                                | bottomLeft =
                                    existingSubClusters.bottomLeft
                                        |> insert (Vector2.setY y position) id
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
                                        |> insert (vec2 x y) id
                            }
            in
            Cluster
                size
                (IntDict.insert id position entities)
                subClusters


remove : Id -> Cluster -> Cluster
remove id cluster =
    case cluster of
        Empty _ ->
            cluster

        Singleton size existingEntities position ->
            let
                rest =
                    Set.remove id existingEntities
            in
            if Set.isEmpty rest then
                Empty size

            else
                Singleton size rest position

        Cluster size entities existingSubClusters ->
            case IntDict.get id entities of
                Nothing ->
                    cluster

                Just position ->
                    let
                        remainingEntities =
                            IntDict.remove id entities
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
                            Singleton size (Set.singleton lastEntity) lastPosition

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
                                                        |> remove id
                                            }

                                        ( False, True ) ->
                                            { existingSubClusters
                                                | topRight =
                                                    existingSubClusters.topRight
                                                        |> remove id
                                            }

                                        ( True, False ) ->
                                            { existingSubClusters
                                                | bottomLeft =
                                                    existingSubClusters.bottomLeft
                                                        |> remove id
                                            }

                                        ( False, False ) ->
                                            { existingSubClusters
                                                | bottomRight =
                                                    existingSubClusters.bottomRight
                                                        |> remove id
                                            }
                            in
                            Cluster
                                size
                                remainingEntities
                                subClusters


location : Id -> Cluster -> Maybe Vec2
location id cluster =
    case cluster of
        Empty _ ->
            Nothing

        Singleton _ entities position ->
            if Set.member id entities then
                Just position

            else
                Nothing

        Cluster _ entities _ ->
            IntDict.get id entities


clusters : Float -> Vec2 -> Cluster -> List ( Vec2, List Id )
clusters precision viewpoint cluster =
    case cluster of
        Empty _ ->
            []

        Singleton _ entities position ->
            [ ( position, Set.toList entities ) ]

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
