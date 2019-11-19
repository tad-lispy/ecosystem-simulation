module Stats exposing
    ( DataPoint
    , DataPoints
    , Stats
    , appendDataPoints
    , empty
    )

import Dict exposing (Dict)


type alias DataPoints =
    Dict String (List DataPoint)


type alias DataPoint =
    { time : Float
    , quantity : Float
    }


type alias Stats =
    Dict String Float


empty : DataPoints
empty =
    Dict.empty


appendDataPoints : Float -> Stats -> DataPoints -> DataPoints
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
