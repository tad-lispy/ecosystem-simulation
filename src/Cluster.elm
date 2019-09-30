module Cluster exposing
    ( Cluster(..)
    , Coordinates(..)
    , clusters
    , insert
    , location
    , locations
    , remove
    )

import IntDict exposing (IntDict)
import Length exposing (Length, Meters)
import Point2d exposing (Point2d)
import Quantity exposing (zero)
import Set exposing (Set)
import Vector2d exposing (Vector2d)


type Coordinates
    = Global
    | Local (Vector2d Meters Coordinates)


type Cluster
    = Empty Length
    | Singleton Length (Set Id) (Point2d Meters Coordinates)
    | Cluster Length (IntDict (Point2d Meters Coordinates)) SubClusters


type alias Group =
    { members : Set Id
    , location : Point2d Meters Coordinates
    }


type alias Id =
    Int


type alias SubClusters =
    { topLeft : Cluster
    , topRight : Cluster
    , bottomLeft : Cluster
    , bottomRight : Cluster
    }



{- TODO: The Quadrant type seems useful, but is currently unused. Either start using it or remove dead code -}


type Quadrant
    = TopLeft
    | TopRight
    | BottomLeft
    | BottomRight


quadrant : Point2d Meters Coordinates -> Length -> Quadrant
quadrant position size =
    let
        half : Length
        half =
            Quantity.half size
    in
    case
        ( Point2d.xCoordinate position
            |> Quantity.lessThan half
        , Point2d.yCoordinate position
            |> Quantity.lessThan half
        )
    of
        ( True, True ) ->
            TopLeft

        ( True, False ) ->
            TopRight

        ( False, True ) ->
            BottomLeft

        ( False, False ) ->
            BottomRight


insert :
    Point2d Meters Coordinates
    -> Id
    -> Cluster
    -> Cluster
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
                    position

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
                        size
                            |> Quantity.half
                            |> Empty
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
                half : Length
                half =
                    Quantity.half size

                subClusters =
                    case
                        ( Point2d.xCoordinate position
                            |> Quantity.lessThan half
                        , Point2d.yCoordinate position
                            |> Quantity.lessThan half
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
                                    position
                                        |> Point2d.xCoordinate
                                        |> Quantity.minus half

                                y =
                                    position
                                        |> Point2d.yCoordinate
                            in
                            { existingSubClusters
                                | topRight =
                                    existingSubClusters.topRight
                                        |> insert (Point2d.xy x y) id
                            }

                        ( True, False ) ->
                            let
                                x =
                                    position
                                        |> Point2d.xCoordinate

                                y =
                                    position
                                        |> Point2d.yCoordinate
                                        |> Quantity.minus half
                            in
                            { existingSubClusters
                                | bottomLeft =
                                    existingSubClusters.bottomLeft
                                        |> insert (Point2d.xy x y) id
                            }

                        ( False, False ) ->
                            let
                                x : Length
                                x =
                                    position
                                        |> Point2d.xCoordinate
                                        |> Quantity.minus half

                                y =
                                    position
                                        |> Point2d.yCoordinate
                                        |> Quantity.minus half
                            in
                            { existingSubClusters
                                | bottomRight =
                                    existingSubClusters.bottomRight
                                        |> insert (Point2d.xy x y) id
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
                                half : Length
                                half =
                                    Quantity.half size

                                subClusters =
                                    case
                                        ( Point2d.xCoordinate position
                                            |> Quantity.lessThan half
                                        , Point2d.yCoordinate position
                                            |> Quantity.lessThan half
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


locations :
    Cluster
    -> IntDict (Point2d Meters Coordinates)
locations cluster =
    case cluster of
        Empty _ ->
            IntDict.empty

        Singleton _ ids position ->
            Set.foldl
                (\id -> IntDict.insert id position)
                IntDict.empty
                ids

        Cluster _ ids _ ->
            ids


location :
    Id
    -> Cluster
    -> Maybe (Point2d Meters Coordinates)
location id cluster =
    cluster
        |> locations
        |> IntDict.get id


clusters :
    Float
    -> Point2d Meters Coordinates
    -> Cluster
    -> List Group
clusters precision viewpoint cluster =
    case cluster of
        Empty _ ->
            []

        Singleton _ ids position ->
            [ { location = position
              , members = ids
              }
            ]

        Cluster size ids subClusters ->
            let
                half =
                    Quantity.half size

                center =
                    Point2d.xy half half

                distance =
                    Point2d.distanceFrom viewpoint center
            in
            if Quantity.ratio size distance < precision then
                [ { location = center
                  , members =
                        ids
                            |> IntDict.keys
                            |> Set.fromList
                  }
                ]

            else
                let
                    topRightTranslation : Vector2d Meters Coordinates
                    topRightTranslation =
                        Vector2d.xy half Quantity.zero

                    bottomLeftTranslation : Vector2d Meters Coordinates
                    bottomLeftTranslation =
                        Vector2d.xy zero half

                    bottomRightTranslation : Vector2d Meters Coordinates
                    bottomRightTranslation =
                        Vector2d.xy half half
                in
                [ subClusters.topLeft
                    |> clusters
                        precision
                        viewpoint
                , subClusters.topRight
                    |> clusters
                        precision
                        (Point2d.translateBy
                            (Vector2d.reverse topRightTranslation)
                            viewpoint
                        )
                    |> List.map
                        (\group ->
                            { members = group.members
                            , location =
                                Point2d.translateBy
                                    topRightTranslation
                                    group.location
                            }
                        )
                , subClusters.bottomLeft
                    |> clusters
                        precision
                        (Point2d.translateBy
                            (Vector2d.reverse bottomLeftTranslation)
                            viewpoint
                        )
                    |> List.map
                        (\group ->
                            { members = group.members
                            , location =
                                Point2d.translateBy
                                    bottomLeftTranslation
                                    group.location
                            }
                        )
                , subClusters.bottomRight
                    |> clusters
                        precision
                        (Point2d.translateBy
                            (Vector2d.reverse bottomRightTranslation)
                            viewpoint
                        )
                    |> List.map
                        (\group ->
                            { members = group.members
                            , location =
                                Point2d.translateBy
                                    bottomRightTranslation
                                    group.location
                            }
                        )
                ]
                    |> List.concat
