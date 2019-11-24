module Stats exposing
    ( DataPoint
    , DataPoints
    , Stats
    , appendDataPoints
    , dropOlderThan
    , empty
    )

import Dict exposing (Dict)
import Duration exposing (Duration)
import List.Extra as List
import Quantity


type alias DataPoints =
    Dict String (List DataPoint)


type alias DataPoint =
    { time : Duration
    , quantity : Float
    }


type alias Stats =
    Dict String Float


empty : DataPoints
empty =
    Dict.empty


appendDataPoints : Duration -> Stats -> DataPoints -> DataPoints
appendDataPoints time stats dataPoints =
    stats
        |> Dict.foldl
            (\label quantity memo ->
                Dict.update label
                    (\points ->
                        points
                            |> Maybe.withDefault []
                            |> (::) (DataPoint time quantity)
                            |> Just
                    )
                    memo
            )
            dataPoints


dropOlderThan : Duration -> DataPoints -> DataPoints
dropOlderThan threshold data =
    data
        |> Dict.map
            (\_ points ->
                List.takeWhile
                    (.time >> Quantity.greaterThan threshold)
                    points
            )
