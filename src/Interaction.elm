module Interaction exposing
    ( Interaction
    , Register
    , batch
    , clear
    , register
    )

import IntDict exposing (IntDict)


type alias Interaction action =
    { action : action
    , other : Id
    }


type alias Register action =
    IntDict (List (Interaction action))


type alias Id =
    Int


register :
    Id
    -> Interaction action
    -> Register action
    -> Register action
register this { other, action } register_ =
    let
        interactions =
            register_
                |> IntDict.get other
                |> Maybe.withDefault []
                |> (::) { other = this, action = action }
    in
    IntDict.insert other interactions register_


batch :
    Id
    -> List (Interaction action)
    -> Register action
    -> Register action
batch id interactions register_ =
    List.foldl (register id)
        register_
        interactions


clear :
    Id
    -> Register action
    -> Register action
clear id register_ =
    IntDict.remove id register_
