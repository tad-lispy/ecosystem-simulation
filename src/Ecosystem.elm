module Ecosystem exposing
    ( Duration
    , Group
    , Id
    , Image
    , Interaction
    , Setup
    , Update
    , simulation
    )

import AltMath.Vector2 as Vector2 exposing (Vec2, vec2)
import Browser
import Browser.Events
import Color exposing (Color)
import Element exposing (Element)
import Html exposing (Html)
import IntDict exposing (IntDict)
import List.Extra as List
import WrappedPlane exposing (Plane)


simulation :
    Setup entity action
    -> Program Flags (Model entity action) Msg
simulation setup =
    let
        main : Program Flags (Model entity action) Msg
        main =
            Browser.element
                { init = init setup
                , view = view
                , update = update
                , subscriptions = subscriptions
                }
    in
    main


type alias Setup entity action =
    { update :
        Id
        -> entity
        -> List (Interaction action)
        -> List (Group entity)
        -> Duration
        -> Update entity action
    , init : List (Update entity action)
    , view : entity -> Image
    , size : Float
    }


type alias Flags =
    ()


type alias Model entity action =
    { surface : Plane entity
    , interactions : InteractionsRegister action
    }


type alias Id =
    Int


type alias Interaction action =
    { other : Id
    , action : action
    }


type alias Group entity =
    { members : List ( Id, entity )
    , position : Vec2
    }


type alias Image =
    { fill : Color
    , stroke : Color
    , size : Float
    }


type alias Update entity action =
    { this : Maybe entity
    , interactions : List (Interaction action)
    , movement : Vec2
    }


type alias Duration =
    Float


init :
    Setup entity action
    -> Flags
    -> ( Model entity action, Cmd Msg )
init setup _ =
    let
        empty : Model entity surface
        empty =
            { surface = WrappedPlane.empty setup.size
            , interactions = IntDict.empty
            }

        initReducer :
            Int
            -> Update entity action
            -> Model entity action
            -> Model entity action
        initReducer id { this, interactions, movement } model =
            case this of
                Nothing ->
                    model

                Just entity ->
                    { model
                        | surface =
                            WrappedPlane.place id entity model.surface
                        , interactions =
                            List.foldl
                                (registerInteraction id)
                                model.interactions
                                interactions
                    }

        registerInteraction :
            Id
            -> Interaction action
            -> InteractionsRegister action
            -> InteractionsRegister action
        registerInteraction actor { other, action } register =
            let
                interactions =
                    register
                        |> IntDict.get other
                        |> Maybe.withDefault []
                        |> (::) { other = actor, action = action }
            in
            IntDict.insert other interactions register
    in
    ( List.indexedFoldl initReducer empty setup.init
    , Cmd.none
    )


type alias InteractionsRegister action =
    IntDict (List (Interaction action))


type Msg
    = Animate Float


update :
    Msg
    -> Model entity action
    -> ( Model entity action, Cmd Msg )
update msg model =
    case msg of
        Animate duration ->
            ( model
            , Cmd.none
            )


subscriptions : Model entity action -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrameDelta Animate


view : Model entity action -> Html Msg
view model =
    Element.text "Hello, System!"
        |> Element.layout []
