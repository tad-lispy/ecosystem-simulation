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
    this.entities
        |> IntDict.toList
        |> List.map
            (Tuple.mapSecond (Vector2d.from this.cursor))
        |> List.sortBy
            (Tuple.second >> Vector2d.squaredLength)
        |> List.take limit


render : Float -> Float -> Surface -> List ( Id, Point2d )
render width height (Surface this) =
    let
        translation =
            this.cursor
                |> Vector2d.from Point2d.origin
    in
    this.entities
        |> IntDict.map
            (\_ point ->
                point
                    |> Point2d.translateBy translation
                    |> normalize this.size
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
                    |> normalize this.size
        }


shiftTo : Id -> Surface -> Maybe Surface
shiftTo id (Surface this) =
    this.entities
        |> IntDict.get id
        |> Maybe.map
            (\point -> { this | cursor = point })
        |> Maybe.map Surface


normalize : Float -> Point2d -> Point2d
normalize size point =
    Point2d.fromCoordinates
        ( fractionalModBy size (Point2d.xCoordinate point)
        , fractionalModBy size (Point2d.yCoordinate point)
        )
