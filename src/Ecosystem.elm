module Ecosystem exposing
    ( ActorUpdate
    , Change(..)
    , Coordinates
    , Id
    , Image
    , Program
    , Setup
    , Spawn
    , grid
    , simulation
    )

import Browser
import Browser.Events
import Cluster exposing (Coordinates)
import Color exposing (Color)
import Dict exposing (Dict)
import Duration exposing (Duration, Seconds)
import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import Environment exposing (Environment)
import Html exposing (Html)
import Html.Attributes
import IntDict exposing (IntDict)
import Interaction exposing (Interaction)
import Json.Decode
import Length exposing (Length, Meters)
import LineChart
import LineChart.Area
import LineChart.Axis
import LineChart.Axis.Intersection
import LineChart.Colors
import LineChart.Container
import LineChart.Dots
import LineChart.Events
import LineChart.Grid
import LineChart.Interpolation
import LineChart.Junk
import LineChart.Legends
import LineChart.Line
import List.Extra as List
import Maybe.Extra as Maybe
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate, zero)
import Speed exposing (MetersPerSecond, Speed)
import Stats exposing (DataPoint, DataPoints, Stats)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Time
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
        Id
        -> actor
        -> Environment actor action
        -> ActorUpdate actor action
    , init : List (Spawn actor action)
    , paintActor : actor -> Image
    , size : Length
    , gatherStats : List actor -> Stats
    }


type alias Flags =
    ()


type alias Model actor action =
    { surface : Plane
    , interactions : Interaction.Register action
    , paused : Bool
    , actors : IntDict actor
    , seed : Id
    , selected : Maybe Id
    , clock : Float
    , stats : Stats
    , dataPoints : DataPoints
    }


type alias Id =
    Int


type alias Image =
    { fill : Color
    , stroke : Color
    , size : Length
    }


type alias Spawn actor action =
    { actor : actor
    , interactions : List (Interaction action)
    , displacement : Vector2d Meters Coordinates
    }


type alias ActorUpdate actor action =
    { change : Change actor
    , interactions : List (Interaction action)
    , velocity : Vector2d MetersPerSecond Coordinates
    , spawn : List (Spawn actor action)
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
        model =
            List.indexedFoldl initReducer empty setup.init

        initReducer :
            Int
            -> Spawn actor action
            -> Model actor action
            -> Model actor action
        initReducer id spawn memo =
            { memo
                | surface =
                    memo.surface
                        |> WrappedPlane.shift spawn.displacement
                        |> WrappedPlane.place id
                        |> WrappedPlane.return
                , actors =
                    memo.actors
                        |> IntDict.insert id spawn.actor
                , interactions =
                    List.foldl
                        (Interaction.register id)
                        memo.interactions
                        spawn.interactions
                , seed = id + 1
            }

        empty : Model actor action
        empty =
            { surface = WrappedPlane.empty setup.size
            , interactions = IntDict.empty
            , actors = IntDict.empty
            , seed = 0
            , paused = False
            , selected = Nothing
            , clock = 0
            , stats = Dict.empty
            , dataPoints = Stats.empty
            }

        stats =
            model.actors
                |> IntDict.values
                |> setup.gatherStats

        dataPoints =
            Stats.appendDataPoints 0 stats Stats.empty
    in
    ( { model | stats = stats, dataPoints = dataPoints }
    , Cmd.none
    )


type Msg
    = Animate Float
    | ClockTick Time.Posix
    | Selected Id
    | Pause
    | Play


update :
    Setup actor action
    -> Msg
    -> Model actor action
    -> ( Model actor action, Cmd Msg )
update setup msg model =
    case msg of
        Animate delta ->
            let
                _ =
                    model.actors
                        |> IntDict.size

                latency : Duration
                latency =
                    -- If the frame rate drops below 30fps then slow down the
                    -- animation but retain precision. Otherwise there is too
                    -- much noise and things get wild.
                    delta
                        |> min 32
                        |> Duration.milliseconds

                updateActor :
                    Id
                    -> actor
                    -> ActorUpdate actor action
                updateActor id actor =
                    case WrappedPlane.shiftTo id model.surface of
                        Nothing ->
                            -- This should never happen
                            { change = Removed
                            , velocity = Vector2d.zero
                            , interactions = []
                            , spawn = []
                            }

                        Just plane ->
                            model.interactions
                                |> IntDict.get id
                                |> Maybe.withDefault []
                                |> Environment.create
                                    model.actors
                                    (plane |> WrappedPlane.remove id)
                                    latency
                                |> setup.updateActor id actor
            in
            ( model.actors
                |> IntDict.map updateActor
                |> IntDict.foldl (applyActorUpdate latency) { model | interactions = IntDict.empty }
                |> resetAnchor
            , Cmd.none
            )

        ClockTick _ ->
            let
                clock =
                    model.clock + 1000

                stats =
                    model.actors
                        |> IntDict.values
                        |> setup.gatherStats

                dataPoints =
                    Stats.appendDataPoints clock stats model.dataPoints
            in
            ( { model
                | clock = clock
                , stats = stats
                , dataPoints = dataPoints
              }
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

        Play ->
            ( { model | paused = False }
            , Cmd.none
            )


subscriptions : Model actor action -> Sub Msg
subscriptions model =
    if model.paused then
        Sub.none

    else
        [ Browser.Events.onAnimationFrameDelta Animate
        , Time.every 1000 ClockTick
        ]
            |> Sub.batch


view : Setup actor action -> Model actor action -> Html Msg
view setup model =
    let
        scene : Element Msg
        scene =
            model
                |> paintScene setup resolution
                |> Svg.svg
                    [ Html.Attributes.style "width" "100%"
                    , Html.Attributes.style "height" "100%"
                    , Svg.Attributes.viewBox viewbox
                    , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
                    , Html.Attributes.style "background" <|
                        Color.toCssString <|
                            Color.hsl 0.6 0.8 0.1
                    , Svg.Events.onClick
                        (if model.paused then
                            Animate 16

                         else
                            Pause
                        )
                    ]
                |> Element.html

        stats : Element Msg
        stats =
            [ statsUi model
            , model.stats
                |> Dict.map (\key value -> String.fromFloat value)
                |> Dict.toList
                |> List.map (\( key, value ) -> key ++ ": " ++ value)
                |> List.map Element.text
                |> Element.column []
            ]
                |> Element.column [ Element.width Element.fill ]

        controls : Element Msg
        controls =
            if model.paused then
                Input.button
                    [ Font.color (Element.rgb 1 1 1)
                    , Element.centerX
                    , Element.centerY
                    ]
                    { onPress =
                        Just
                            (if model.paused then
                                Play

                             else
                                Pause
                            )
                    , label =
                        Element.text
                            (if model.paused then
                                "▷"

                             else
                                "❙❙"
                            )
                    }

            else
                Element.none

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
    scene
        |> Element.layout
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.inFront controls
            , Element.below stats
            ]


statsUi : Model actor action -> Element Msg
statsUi model =
    let
        dataPoints =
            Stats.appendDataPoints model.clock model.stats model.dataPoints

        chartConfig : LineChart.Config DataPoint Msg
        chartConfig =
            { y =
                LineChart.Axis.default 400
                    "Quantity"
                    .quantity
            , x =
                LineChart.Axis.default 700
                    "Time"
                    (.time >> (\t -> t / 60000))
            , container =
                LineChart.Container.custom
                    { attributesHtml =
                        [ Html.Attributes.style "font-family" "monospace"
                        ]
                    , attributesSvg =
                        [ Html.Attributes.style "font-size" "12px"
                        ]
                    , size = LineChart.Container.relative
                    , margin = LineChart.Container.Margin 30 100 60 80
                    , id = "population-chart"
                    }
            , interpolation = LineChart.Interpolation.monotone
            , intersection = LineChart.Axis.Intersection.default
            , legends =
                LineChart.Legends.byBeginning
                    (LineChart.Junk.label LineChart.Colors.gray)
            , events = LineChart.Events.default
            , junk = LineChart.Junk.default
            , grid = LineChart.Grid.default
            , area = LineChart.Area.default
            , line = LineChart.Line.default
            , dots = LineChart.Dots.default
            }

        lines =
            model.dataPoints
                |> Dict.toList
                |> List.map
                    (\( label, points ) ->
                        LineChart.line
                            LineChart.Colors.green
                            LineChart.Dots.none
                            label
                            points
                    )
    in
    lines
        |> LineChart.viewCustom chartConfig
        |> Element.html
        |> Element.el
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
    -> List (Spawn actor action)
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
                { actor = actor
                , interactions = []
                , displacement = Vector2d.xy x y
                }
            )


applyActorUpdate :
    Duration
    -> Id
    -> ActorUpdate actor action
    -> Model actor action
    -> Model actor action
applyActorUpdate latency id actorUpdate model =
    let
        withActorUpdated =
            case actorUpdate.change of
                Unchanged ->
                    { model
                        | actors =
                            model.actors
                        , surface =
                            model.surface
                                |> WrappedPlane.shiftTo id
                                |> Maybe.map
                                    (actorUpdate.velocity
                                        |> Vector2d.for latency
                                        |> WrappedPlane.shift
                                    )
                                |> Maybe.map (WrappedPlane.place id)
                                |> Maybe.withDefault model.surface
                        , interactions =
                            model.interactions
                                |> Interaction.batch id actorUpdate.interactions
                    }

                Changed actor ->
                    { model
                        | actors =
                            model.actors
                                |> IntDict.insert id actor
                        , surface =
                            model.surface
                                |> WrappedPlane.shiftTo id
                                |> Maybe.map
                                    (actorUpdate.velocity
                                        |> Vector2d.for latency
                                        |> WrappedPlane.shift
                                    )
                                |> Maybe.map (WrappedPlane.place id)
                                |> Maybe.withDefault model.surface
                        , interactions =
                            model.interactions
                                |> Interaction.batch id actorUpdate.interactions
                    }

                Removed ->
                    { model
                        | actors =
                            IntDict.remove id model.actors
                        , surface =
                            WrappedPlane.remove id model.surface
                        , interactions =
                            model.interactions
                                |> Interaction.batch id actorUpdate.interactions
                    }
    in
    withActorUpdated
        |> addSpawns actorUpdate.spawn


addSpawns :
    List (Spawn actor action)
    -> Model actor action
    -> Model actor action
addSpawns spawns model =
    spawns
        |> List.foldl addSpawn { model | surface = WrappedPlane.placeAnchor model.surface }
        |> resetAnchor


addSpawn :
    Spawn actor action
    -> Model actor action
    -> Model actor action
addSpawn spawn model =
    { model
        | actors = IntDict.insert model.seed spawn.actor model.actors
        , surface =
            model.surface
                |> WrappedPlane.shift spawn.displacement
                |> WrappedPlane.place model.seed
                |> WrappedPlane.return
        , seed = model.seed + 1
    }


resetAnchor :
    Model actor action
    -> Model actor action
resetAnchor model =
    { model
        | surface =
            model.surface
                |> WrappedPlane.return
                |> WrappedPlane.removeAnchor
    }

