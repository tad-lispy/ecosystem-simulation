module Surface exposing
    ( Surface
    , anchor
    , clusters
    , empty
    , grid
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
import Set exposing (Set)


type Surface
    = Surface
        { cluster : Cluster
        , size : Float
        , cursor : Vec2
        , anchor : Vec2
        }


type alias Id =
    Int


empty : Float -> Surface
empty size =
    Surface
        { cluster = Empty size
        , cursor = vec2 0 0
        , anchor = vec2 0 0
        , size = size
        }


anchor : Surface -> Surface
anchor (Surface this) =
    Surface { this | anchor = this.cursor }


return : Surface -> Surface
return (Surface this) =
    Surface { this | cursor = this.anchor }


place : Id -> Surface -> Surface
place id (Surface this) =
    Surface
        { this
            | cluster =
                this.cluster
                    |> Cluster.remove id
                    |> Cluster.insert this.cursor id
        }


remove : Id -> Surface -> Surface
remove id (Surface this) =
    Surface
        { this
            | cluster =
                Cluster.remove id this.cluster
        }


render : Float -> Float -> Surface -> List ( Id, Vec2 )
render width height (Surface this) =
    case this.cluster of
        Cluster.Empty _ ->
            []

        Cluster.Singleton _ entities location ->
            entities
                |> Set.toList
                |> List.map (\entity -> ( entity, location ))

        Cluster viewport entities subClusters ->
            entities
                |> IntDict.toList


shift : Vec2 -> Surface -> Surface
shift vector (Surface this) =
    Surface
        { this
            | cursor =
                this.cursor
                    |> Vector2.add vector
                    |> wrap this.size
        }


shiftTo : Id -> Surface -> Maybe Surface
shiftTo id (Surface this) =
    this.cluster
        |> Cluster.location id
        |> Maybe.map
            (\point -> { this | cursor = point })
        |> Maybe.map Surface


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


clusters : Float -> Surface -> List ( Vec2, List Id )
clusters precision (Surface universe) =
    let
        internal origin cluster =
            case cluster of
                Empty _ ->
                    []

                Singleton _ entities location ->
                    [ ( from universe.size origin location, Set.toList entities ) ]

                Cluster size entities subClusters ->
                    let
                        center =
                            vec2 (size / 2) (size / 2)

                        position =
                            from universe.size origin center

                        distance =
                            Vector2.length position
                    in
                    if size / distance < precision then
                        [ ( position, IntDict.keys entities ) ]

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


grid : Int -> Int -> Float -> Float -> ( List Int, Surface )
grid rows cols distance size =
    let
        entities =
            List.range 0 (rows * cols - 1)

        surface =
            entities
                |> List.foldl
                    (\entity memo ->
                        let
                            x =
                                (modBy cols entity
                                    |> toFloat
                                    |> (*) distance
                                )
                                    + 100

                            y =
                                (entity // cols)
                                    |> toFloat
                                    |> (*) distance
                                    |> (+) 300
                        in
                        memo
                            |> shift (vec2 x y)
                            |> place entity
                            |> return
                    )
                    (empty size)
    in
    ( entities, surface )
