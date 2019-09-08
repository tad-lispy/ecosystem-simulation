module Surface exposing
    ( Surface
    , clusters
    , empty
    , place
    , remove
    , render
    , shift
    , shiftTo
    )

import Basics.Extra exposing (fractionalModBy)
import IntDict exposing (IntDict)
import Math.Vector2 as Vector2 exposing (Vec2, vec2)


type Surface
    = Surface
        { size : Float
        , cursor : Vec2
        , entities : IntDict Vec2
        }


type alias Id =
    Int


empty : Float -> Surface
empty size =
    Surface
        { size = size
        , cursor = vec2 0 0
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


entities : Int -> Surface -> List ( Id, Vec2 )
entities limit (Surface this) =
    this.entities
        |> IntDict.map
            (\_ point ->
                point
                    |> Vector2.sub this.cursor
                    |> wrap this.size
            )
        |> IntDict.toList
        |> List.sortBy
            (Tuple.second >> Vector2.lengthSquared)
        |> List.take limit


render : Float -> Float -> Surface -> List ( Id, Vec2 )
render width height (Surface this) =
    this.entities
        |> IntDict.map
            (\_ point ->
                point
                    |> Vector2.sub this.cursor
                    |> wrap this.size
            )
        |> IntDict.filter
            (\_ point ->
                (Vector2.getX point < width)
                    && (Vector2.getY point < height)
            )
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
wrap : Float -> Vec2 -> Vec2
wrap size point =
    vec2
        (fractionalModBy size (Vector2.getX point + size / 2) - (size / 2))
        (fractionalModBy size (Vector2.getY point + size / 2) - (size / 2))
