module WrappedPlane exposing
    ( DCluster
    , Group
    , Id
    , Plane
    , clusters
    , debug
    , empty
    , foldl
    , place
    , placeAnchor
    , remove
    , removeAnchor
    , render
    , return
    , shift
    , shiftTo
    , toList
    )

import Basics.Extra exposing (fractionalModBy)
import Cluster exposing (Cluster(..), Coordinates(..))
import IntDict exposing (IntDict)
import Length exposing (Length, Meters)
import Maybe.Extra as Maybe
import Point2d exposing (Point2d)
import Quantity exposing (zero)
import Set exposing (Set)
import Vector2d exposing (Vector2d)


type Plane
    = Plane
        { cluster : Cluster
        , size : Length
        , cursor : Point2d Meters Coordinates
        , anchors : List (Point2d Meters Coordinates)
        }


type alias Id =
    Int


type alias Group =
    { members : Set Id
    , position : Vector2d Meters Coordinates
    }


empty : Length -> Plane
empty size =
    Plane
        { cluster = Empty size
        , cursor = Point2d.origin
        , anchors = []
        , size = size
        }


placeAnchor : Plane -> Plane
placeAnchor (Plane this) =
    Plane { this | anchors = this.cursor :: this.anchors }


removeAnchor : Plane -> Plane
removeAnchor (Plane this) =
    Plane { this | anchors = List.drop 1 this.anchors }


return : Plane -> Plane
return (Plane this) =
    Plane
        { this
            | cursor =
                this.anchors
                    |> List.head
                    |> Maybe.withDefault Point2d.origin
        }


place :
    Id
    -> Plane
    -> Plane
place id (Plane this) =
    let
        cluster =
            this.cluster
                |> Cluster.remove id
                |> Cluster.insert this.cursor id
    in
    Plane
        { this
            | cluster = cluster
        }


remove : Id -> Plane -> Plane
remove id (Plane this) =
    Plane
        { this
            | cluster =
                Cluster.remove id this.cluster
        }


toList : Plane -> List Id
toList (Plane this) =
    Cluster.locations this.cluster
        |> IntDict.keys


foldl :
    (Id -> Plane -> a -> a)
    -> a
    -> Plane
    -> a
foldl reducer initial (Plane this) =
    this.cluster
        |> Cluster.locations
        |> IntDict.foldl
            (\id location ->
                { this | cursor = location }
                    |> Plane
                    |> reducer id
            )
            initial


render :
    Float
    -> Float
    -> Plane
    -> List ( Id, Vector2d Meters Coordinates )
render width height (Plane this) =
    this.cluster
        |> Cluster.locations
        |> IntDict.toList
        |> List.map (Tuple.mapSecond (from this.size this.cursor))


shift :
    Vector2d Meters Coordinates
    -> Plane
    -> Plane
shift vector (Plane this) =
    Plane
        { this
            | cursor =
                this.cursor
                    |> Point2d.translateBy vector
                    |> wrap this.size
        }


shiftTo : Id -> Plane -> Maybe Plane
shiftTo id (Plane this) =
    this.cluster
        |> Cluster.location id
        |> Maybe.map
            (\point ->
                { this
                    | cursor = point
                }
            )
        |> Maybe.map Plane


wrap : Length -> Point2d Meters Coordinates -> Point2d Meters Coordinates
wrap size point =
    let
        x : Length
        x =
            point
                |> Point2d.xCoordinate
                |> Length.inMeters
                |> fractionalModBy (Length.inMeters size)
                |> Length.meters

        y : Length
        y =
            point
                |> Point2d.yCoordinate
                |> Length.inMeters
                |> fractionalModBy (Length.inMeters size)
                |> Length.meters
    in
    Point2d.xy x y


clusters :
    Float
    -> Plane
    -> List Group
clusters precision (Plane universe) =
    let
        internal :
            Point2d Meters Coordinates
            -> Cluster
            -> List Group
        internal viewpoint cluster =
            case cluster of
                Empty _ ->
                    []

                Singleton _ ids location ->
                    [ { position = from universe.size viewpoint location
                      , members = ids
                      }
                    ]

                Cluster size locations subClusters ->
                    let
                        half =
                            Quantity.half size

                        center =
                            Point2d.xy half half

                        position =
                            from universe.size viewpoint center

                        distance =
                            Vector2d.length position

                        ids =
                            locations
                                |> IntDict.keys
                                |> Set.fromList
                    in
                    if Quantity.ratio size distance < precision then
                        [ { position = position
                          , members = ids
                          }
                        ]

                    else
                        let
                            topRightTranslation : Vector2d Meters Coordinates
                            topRightTranslation =
                                Vector2d.xy half zero

                            bottomLeftTranslation : Vector2d Meters Coordinates
                            bottomLeftTranslation =
                                Vector2d.xy zero half

                            bottomRightTranslation : Vector2d Meters Coordinates
                            bottomRightTranslation =
                                Vector2d.xy half half
                        in
                        [ subClusters.topLeft
                            |> internal
                                viewpoint
                        , subClusters.topRight
                            |> internal
                                (Point2d.translateBy
                                    (Vector2d.reverse topRightTranslation)
                                    viewpoint
                                )
                        , subClusters.bottomLeft
                            |> internal
                                (Point2d.translateBy
                                    (Vector2d.reverse bottomLeftTranslation)
                                    viewpoint
                                )
                        , subClusters.bottomRight
                            |> internal
                                (Point2d.translateBy
                                    (Vector2d.reverse bottomRightTranslation)
                                    viewpoint
                                )
                        ]
                            |> List.concat
    in
    internal universe.cursor universe.cluster


from :
    Length
    -> Point2d Meters Coordinates
    -> Point2d Meters Coordinates
    -> Vector2d Meters Coordinates
from size start end =
    let
        half : Length
        half =
            Quantity.half size

        halfway =
            Vector2d.xy half half

        halfwayBack =
            Vector2d.reverse halfway

        back =
            Vector2d.from start Point2d.origin
    in
    end
        |> Point2d.translateBy halfway
        |> Point2d.translateBy back
        |> wrap size
        |> Point2d.translateBy halfwayBack
        |> Vector2d.from Point2d.origin



-- Debug utilities


type DCluster
    = DCluster
        { topLeft : DCluster
        , topRight : DCluster
        , bottomLeft : DCluster
        , bottomRight : DCluster
        }
    | DSingleton (List Id)
    | DEmpty


debug : Cluster -> DCluster
debug cluster =
    case cluster of
        Empty _ ->
            DEmpty

        Singleton _ ids _ ->
            DSingleton (ids |> Set.toList)

        Cluster _ locations subClusters ->
            DCluster
                { topLeft = debug subClusters.topLeft
                , topRight = debug subClusters.topRight
                , bottomLeft = debug subClusters.bottomLeft
                , bottomRight = debug subClusters.bottomRight
                }
