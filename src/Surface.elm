module Surface exposing
    ( Surface
    , empty
    , entities
    , place
    , remove
    , render
    , shift
    , shiftTo
    )

import Basics.Extra exposing (fractionalModBy)
import IntDict exposing (IntDict)
import Point2d exposing (Point2d)
import Vector2d exposing (Vector2d)


type Surface
    = Surface
        { size : Float
        , cursor : Point2d
        , entities : IntDict Point2d
        }


type alias Id =
    Int


empty : Float -> Surface
empty size =
    Surface
        { size = size
        , cursor = Point2d.origin
        , entities = IntDict.empty
        }


place : Id -> Surface -> Surface
place id (Surface this) =
    Surface
        { this
            | entities =
                IntDict.insert id this.cursor this.entities
        }


remove : Id -> Surface -> Surface
remove id (Surface this) =
    Surface
        { this
            | entities =
                IntDict.remove id this.entities
        }


entities : Int -> Surface -> List ( Id, Vector2d )
entities limit (Surface this) =
    let
        translation =
            Vector2d.from this.cursor Point2d.origin
    in
    this.entities
        |> IntDict.map
            (\_ point ->
                point
                    |> Point2d.translateBy translation
                    |> wrap this.size
            )
        |> IntDict.toList
        |> List.map
            (Tuple.mapSecond (Vector2d.from Point2d.origin))
        |> List.sortBy
            (Tuple.second >> Vector2d.squaredLength)
        |> List.take limit


render : Float -> Float -> Surface -> List ( Id, Point2d )
render width height (Surface this) =
    let
        translation =
            Vector2d.from this.cursor Point2d.origin
    in
    this.entities
        |> IntDict.map
            (\_ point ->
                point
                    |> Point2d.translateBy translation
                    |> wrap this.size
            )
        |> IntDict.filter
            (\_ point ->
                (Point2d.xCoordinate point < width)
                    && (Point2d.yCoordinate point < height)
            )
        |> IntDict.toList


shift : Vector2d -> Surface -> Surface
shift vector (Surface this) =
    Surface
        { this
            | cursor =
                this.cursor
                    |> Point2d.translateBy vector
                    |> wrap this.size
        }


shiftTo : Id -> Surface -> Maybe Surface
shiftTo id (Surface this) =
    this.entities
        |> IntDict.get id
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
wrap : Float -> Point2d -> Point2d
wrap size point =
    Point2d.fromCoordinates
        ( fractionalModBy size (Point2d.xCoordinate point + size / 2) - (size / 2)
        , fractionalModBy size (Point2d.yCoordinate point + size / 2) - (size / 2)
        )
