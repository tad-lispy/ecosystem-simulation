module Ecosystem exposing
    ( ActorUpdate(..)
    , Change(..)
    , Coordinates
    , Group
    , Id
    , Image
    , Interaction
    , Program
    , Setup
    , grid
    , simulation
    )

import Browser
import Browser.Events
import Cluster exposing (Coordinates)
import Color exposing (Color)
import Duration exposing (Duration, Seconds)
import Element exposing (Element)
import Html exposing (Html)
import Html.Attributes
import IntDict exposing (IntDict)
import Json.Decode
import Length exposing (Length, Meters)
import List.Extra as List
import Maybe.Extra as Maybe
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate, zero)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Transformation exposing (Transformation)
import Vector2d exposing (Vector2d)
import WrappedPlane exposing (Plane)


simulation :
    Setup actor action
    -> Program actor action
simulation setup =
    Browser.element
        { init = init setup
        , view = view setup
        , update = update setup
        , subscriptions = subscriptions
        }


type alias Program actor action =
    Platform.Program Flags (Model actor action) Msg


type alias Resolution =
    Quantity Float (Rate Pixels Meters)


type alias Setup actor action =
    { updateActor :
        (Id -> Maybe actor)
        -> Duration
        -> Id
        -> actor
        -> List Group
        -> List (Interaction action)
        -> ActorUpdate actor action
    , init : List (ActorUpdate actor action)
    , paintActor : actor -> Image
    , size : Length
    }


type alias Flags =
    ()


type alias Model actor action =
    { surface : Plane
    , interactions : InteractionsRegister action
    , paused : Bool
    , actors : IntDict actor
    , selected : Maybe Id
    }


type alias Id =
    Int


type alias Interaction action =
    { other : Id
    , action : action
    }


type alias Group =
    -- Alias and re-expose
    WrappedPlane.Group


type alias Image =
    { fill : Color
    , stroke : Color
    , size : Length
    }


type ActorUpdate actor action
    = ActorUpdate
        { change : Change actor
        , interactions : List (Interaction action)
        , movement : Vector2d Meters Coordinates
        , spawn : List (ActorUpdate actor action)
        }


type alias Coordinates =
    Cluster.Coordinates


type Change actor
    = Unchanged
    | Changed actor
    | Removed


init :
    Setup actor action
    -> Flags
    -> ( Model actor action, Cmd Msg )
init setup _ =
    let
        empty : Model actor action
        empty =
            { surface = WrappedPlane.empty setup.size
            , interactions = IntDict.empty
            , actors = IntDict.empty
            , paused = False
            , selected = Nothing
            }

        initReducer :
            Int
            -> ActorUpdate actor action
            -> Model actor action
            -> Model actor action
        initReducer id (ActorUpdate { change, interactions, spawn, movement }) model =
            case change of
                Unchanged ->
                    model

                Changed actor ->
                    { model
                        | surface =
                            model.surface
                                |> WrappedPlane.shift movement
                                |> WrappedPlane.place id
                                |> WrappedPlane.return
                        , actors =
                            model.actors
                                |> IntDict.insert id actor
                        , interactions =
                            List.foldl
                                (registerInteraction id)
                                model.interactions
                                interactions
                    }

                Removed ->
                    model
    in
    ( List.indexedFoldl initReducer empty setup.init
    , Cmd.none
    )


type alias InteractionsRegister action =
    IntDict (List (Interaction action))


type Msg
    = Animate Float
    | Selected Id
    | Pause


update :
    Setup actor action
    -> Msg
    -> Model actor action
    -> ( Model actor action, Cmd Msg )
update setup msg model =
    case msg of
        Animate delta ->
            let
                duration : Duration
                duration =
                    -- If the frame rate drops below 30fps then slow down the
                    -- animation but retain precision. Otherwise there is too
                    -- much noise and things get wild.
                    delta
                        |> min 32
                        |> Duration.milliseconds

                inspect : Id -> Maybe actor
                inspect id =
                    IntDict.get id model.actors

                updateActor :
                    Id
                    -> actor
                    -> ActorUpdate actor action
                updateActor id actor =
                    case WrappedPlane.shiftTo id model.surface of
                        Nothing ->
                            -- This should never happen
                            ActorUpdate
                                { change = Removed
                                , movement = Vector2d.zero
                                , interactions = []
                                , spawn = []
                                }

                        Just plane ->
                            setup.updateActor
                                inspect
                                duration
                                id
                                actor
                                (WrappedPlane.clusters 0.9 plane)
                                (model.interactions
                                    |> IntDict.get id
                                    |> Maybe.withDefault []
                                )

                applyActorUpdate :
                    Id
                    -> ActorUpdate actor action
                    -> Model actor action
                    -> Model actor action
                applyActorUpdate id (ActorUpdate actorUpdate) memo =
                    case actorUpdate.change of
                        Unchanged ->
                            { memo
                                | actors = memo.actors
                                , surface =
                                    memo.surface
                                        |> WrappedPlane.shiftTo id
                                        |> Maybe.map (WrappedPlane.shift actorUpdate.movement)
                                        |> Maybe.map (WrappedPlane.place id)
                                        |> Maybe.withDefault memo.surface
                                        |> WrappedPlane.return
                            }

                        Changed actor ->
                            { memo
                                | actors =
                                    memo.actors
                                        |> IntDict.insert id actor
                                , surface =
                                    memo.surface
                                        |> WrappedPlane.shiftTo id
                                        |> Maybe.map (WrappedPlane.shift actorUpdate.movement)
                                        |> Maybe.map (WrappedPlane.place id)
                                        |> Maybe.withDefault memo.surface
                                        |> WrappedPlane.return
                            }

                        Removed ->
                            { memo
                                | actors =
                                    IntDict.remove id memo.actors
                                , surface =
                                    WrappedPlane.remove id memo.surface
                            }
            in
            ( model.actors
                |> IntDict.map updateActor
                |> IntDict.foldl applyActorUpdate model
            , Cmd.none
            )

        Selected id ->
            ( { model | selected = Just id }
            , Cmd.none
            )

        Pause ->
            ( { model | paused = True }
            , Cmd.none
            )


subscriptions : Model actor action -> Sub Msg
subscriptions model =
    if model.paused then
        Sub.none

    else
        Browser.Events.onAnimationFrameDelta Animate


view : Setup actor action -> Model actor action -> Html Msg
view setup model =
    let
        resolution =
            Quantity.per
                (Length.meters 1)
                (Pixels.pixels 100)

        viewport =
            setup.size
                |> Quantity.at resolution
                |> Pixels.inPixels

        viewbox =
            [ viewport / -2
            , viewport / -2
            , viewport
            , viewport
            ]
                |> List.map String.fromFloat
                |> String.join " "
    in
    model
        |> paintScene setup resolution
        |> Svg.svg
            [ Html.Attributes.style "width" "100%"
            , Html.Attributes.style "height" "100%"
            , Svg.Attributes.viewBox viewbox
            , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
            , Html.Attributes.style "background" <|
                Color.toCssString <|
                    Color.darkBlue
            , Svg.Events.onClick
                (if model.paused then
                    Animate 16

                 else
                    Pause
                )
            ]
        |> Element.html
        |> Element.layout
            [ Element.width Element.fill
            , Element.height Element.fill
            ]


paintScene :
    Setup actor action
    -> Resolution
    -> Model actor action
    -> List (Svg Msg)
paintScene setup resolution model =
    model.surface
        |> WrappedPlane.render 500 500
        |> List.map
            (\( id, position ) ->
                model.actors
                    |> IntDict.get id
                    |> Maybe.map (paintActor setup resolution id position)
            )
        |> Maybe.values


paintActor :
    Setup actor action
    -> Resolution
    -> Id
    -> Vector2d Meters Coordinates
    -> actor
    -> Svg Msg
paintActor setup resolution id position actor =
    let
        image =
            setup.paintActor actor

        size =
            image.size
                |> Quantity.at resolution
                |> Pixels.inPixels

        strokeWidth =
            size / 5

        translation =
            position
                |> Vector2d.at resolution
                |> Transformation.Translate
    in
    Svg.circle
        [ size
            |> String.fromFloat
            |> Svg.Attributes.r
        , image.fill
            |> Color.toCssString
            |> Svg.Attributes.fill
        , image.stroke
            |> Color.toCssString
            |> Svg.Attributes.stroke
        , strokeWidth
            |> String.fromFloat
            |> Svg.Attributes.strokeWidth
        , translation
            |> Transformation.toString
            |> Svg.Attributes.transform
        , Svg.Events.stopPropagationOn "click"
            (Json.Decode.succeed
                ( Selected id
                , True
                )
            )
        ]
        []



-- Helpers


grid :
    Int
    -> Int
    -> Length
    -> (Int -> actor)
    -> List (ActorUpdate actor action)
grid rows cols distance constructor =
    (rows * cols - 1)
        |> List.range 0
        |> List.map
            (\id ->
                let
                    x =
                        distance
                            |> Quantity.multiplyBy
                                (modBy cols id
                                    |> toFloat
                                )

                    y =
                        distance
                            |> Quantity.multiplyBy
                                ((id // cols)
                                    |> toFloat
                                )

                    actor =
                        constructor id
                in
                ActorUpdate
                    { change = Changed actor
                    , interactions = []
                    , movement = Vector2d.xy x y
                    , spawn = []
                    }
            )


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
