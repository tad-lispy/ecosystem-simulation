module Interaction exposing (Interaction, Register, register)


import IntDict exposing (IntDict)

type alias Interaction action =
    { other : Id
    , action : action
    }


type alias Register action =
    IntDict (List (Interaction action))

type alias Id = Int

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
