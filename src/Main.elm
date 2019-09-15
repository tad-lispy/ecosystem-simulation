module Main exposing (main)

import AltMath.Vector2 as Vector2 exposing (Vec2, vec2)
import Basics.Extra exposing (uncurry)
import Browser
import Browser.Dom as Dom
import Browser.Events
import Circle2d
import Geometry.Svg
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import List.Extra as List
import Maybe.Extra as Maybe
import Point2d
import Result.Extra as Result
import Surface exposing (Surface)
import Svg exposing (Svg)
import Svg.Attributes
import Task exposing (Task)
import Transformation exposing (Transformation)


constants =
    { universeSize = 2000
    , entityMass = 0.01
    , minForce = -0.001
    , repulsionMagnitude = 1.5
    , repulsionScale = 15
    , attractionMagnitude = 1
    , attractionScale = -1
    , maxDelta = 32
    , clusteringPrecision = 0.9
    }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    ()


type alias Model =
    { surface : Surface
    , entities : List Entity
    , selected : Maybe Entity
    , paused : Bool
    , area : Vec2
    }


type alias Entity =
    Int


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        ( entities, surface ) =
            Surface.grid 17 17 173 constants.universeSize
    in
    ( { surface = surface
      , paused = True
      , selected = Nothing
      , area = vec2 constants.universeSize constants.universeSize
      , entities = entities
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Animate Float
    | Click Vec2
    | Insert Vec2 (Result Dom.Error Dom.Element)
    | Remove Entity
    | Select (Maybe Entity)
    | Play
    | Pause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate delta ->
            let
                surface =
                    forces
                        |> List.foldl
                            (\( entity, f ) memo ->
                                let
                                    movement =
                                        Vector2.scale
                                            (virtualDelta / constants.entityMass)
                                            f
                                in
                                memo
                                    |> Surface.shiftTo entity
                                    |> Maybe.map
                                        (Surface.shift movement
                                            >> Surface.place entity
                                        )
                                    |> Maybe.withDefault memo
                            )
                            model.surface

                forces =
                    model.entities
                        |> List.foldl
                            (\entity memo ->
                                case force model.surface entity of
                                    Nothing ->
                                        memo

                                    Just f ->
                                        ( entity, f ) :: memo
                            )
                            []

                virtualDelta : Float
                virtualDelta =
                    -- If the frame rate drops below 30fps then slow down the
                    -- animation but retain precision. Otherwise there is too
                    -- much noise and things get wild.
                    min delta constants.maxDelta
            in
            ( { model
                | surface = surface
              }
            , Cmd.none
            )

        Click location ->
            ( model
            , Dom.getElement "scene"
                |> Task.attempt (Insert location)
            )

        Insert _ (Err _) ->
            ( model
            , Cmd.none
            )

        Insert pointer (Ok dom) ->
            let
                entity =
                    List.length model.entities

                location =
                    vec2
                        (Vector2.getX offset
                            * Vector2.getX model.area
                            / dom.element.width
                        )
                        (Vector2.getY offset
                            * Vector2.getX model.area
                            / dom.element.height
                        )

                offset =
                    Vector2.sub globalOffset element

                globalOffset =
                    Vector2.add pointer viewport

                element =
                    vec2 dom.element.x dom.element.y

                viewport =
                    vec2 dom.viewport.x dom.viewport.y
            in
            ( { model
                | entities =
                    entity :: model.entities
                , surface =
                    Surface.place entity model.surface
              }
            , Cmd.none
            )

        Remove entity ->
            ( { model
                | surface =
                    Surface.remove entity model.surface
              }
            , Cmd.none
            )

        Select selected ->
            ( { model
                | selected = selected
              }
            , Cmd.none
            )

        Play ->
            ( { model | paused = False }
            , Cmd.none
            )

        Pause ->
            ( { model | paused = True }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused then
        Sub.none

    else
        Browser.Events.onAnimationFrameDelta Animate



-- VIEW


view : Model -> Html Msg
view model =
    let
        controls =
            Html.div []
                [ if model.paused then
                    Html.button [ Html.Events.onClick Play ] [ Html.text "Play" ]

                  else
                    Html.button [ Html.Events.onClick Pause ] [ Html.text "Pause" ]
                , Html.button [ Html.Events.onClick (Animate 16) ] [ Html.text "Step" ]
                ]

        scene =
            viewScene
                model.area
                model.selected
                model.surface

        selection =
            case model.selected of
                Just entity ->
                    viewSelection entity model.surface

                Nothing ->
                    Html.div [] []
    in
    Html.div []
        [ controls
        , Html.div [] [ scene, selection ]
        ]


viewScene :
    Vec2
    -> Maybe Entity
    -> Surface
    -> Svg Msg
viewScene area selected surface =
    let
        mousePositionDecoder : (Vec2 -> Msg) -> Decoder Msg
        mousePositionDecoder callback =
            Decode.map2
                vec2
                (Decode.field "clientX" Decode.float)
                (Decode.field "clientY" Decode.float)
                |> Decode.map callback

        viewbox =
            [ 0
            , 0
            , Vector2.getX area
            , Vector2.getY area
            ]
                |> List.map String.fromFloat
                |> String.join " "
    in
    surface
        |> Surface.render (Vector2.getX area) (Vector2.getY area)
        |> List.map viewEntity
        |> Svg.svg
            [ Svg.Attributes.width "50%"
            , Svg.Attributes.height "50%"
            , Svg.Attributes.style "background: black; cursor: crosshair"
            , Svg.Attributes.viewBox viewbox
            , Html.Events.on "click" (mousePositionDecoder Click)
            , Svg.Attributes.id "scene"
            ]


viewSelection : Entity -> Surface -> Svg Msg
viewSelection entity universe =
    Svg.svg
        [ Svg.Attributes.width "50%"
        , Svg.Attributes.height "50%"
        , Svg.Attributes.style "background: black"
        , Svg.Attributes.viewBox "-500 -500 1000 1000"
        , Svg.Attributes.id "selection"
        ]
    <|
        case Surface.shiftTo entity universe of
            Nothing ->
                []

            Just surface ->
                surface
                    |> Surface.clusters constants.clusteringPrecision
                    |> List.map viewInfluence


viewEntity : ( Entity, Vec2 ) -> Svg Msg
viewEntity ( entity, position ) =
    let
        removeDecoder : Decoder ( Msg, Bool )
        removeDecoder =
            Decode.succeed ( Remove entity, True )

        selectionDecoder =
            Decode.succeed ( Select <| Just entity, True )

        color =
            if modBy 2 entity == 0 then
                "blue"

            else
                "red"
    in
    ( Vector2.getX position
    , Vector2.getY position
    )
        |> Point2d.fromCoordinates
        |> Circle2d.withRadius 10
        |> Geometry.Svg.circle2d
            [ Svg.Attributes.fill color
            , Svg.Attributes.style "cursor: pointer"
            , Html.Events.stopPropagationOn "click" selectionDecoder
            ]


viewInfluence : ( Vec2, List Entity ) -> Svg Msg
viewInfluence ( vector, entities ) =
    Svg.g []
        [ Svg.line
            [ Svg.Attributes.x1 "0"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2
                (vector
                    |> Vector2.getX
                    |> String.fromFloat
                )
            , Svg.Attributes.y2
                (vector
                    |> Vector2.getY
                    |> String.fromFloat
                )
            , Svg.Attributes.stroke "white"
            , Svg.Attributes.strokeWidth "1"
            ]
            []
        , Svg.circle
            [ Svg.Attributes.cx
                (vector
                    |> Vector2.getX
                    |> String.fromFloat
                )
            , Svg.Attributes.cy
                (vector
                    |> Vector2.getY
                    |> String.fromFloat
                )
            , Svg.Attributes.r
                (entities
                    |> List.length
                    |> toFloat
                    |> logBase 3
                    |> (*) 3
                    |> String.fromFloat
                )
            , Svg.Attributes.stroke "white"
            , Svg.Attributes.strokeWidth "1"
            , Svg.Attributes.fill "black"
            ]
            []
        ]



-- HELPERS


charge entity =
    if modBy 2 entity == 0 then
        1

    else
        -1


force : Surface -> Entity -> Maybe Vec2
force surface entity =
    let
        addCharge : Entity -> Float -> Float
        addCharge item memo =
            memo + charge item

        addInfluence : ( Vec2, List Entity ) -> Vec2 -> Vec2
        addInfluence ( clusterPosition, clusterEntities ) influence =
            let
                distance =
                    Vector2.lengthSquared clusterPosition

                clusterCharge =
                    clusterEntities
                        |> List.foldl addCharge 0

                attraction =
                    (constants.attractionScale * clusterCharge)
                        / (distance ^ constants.attractionMagnitude)

                repulsion =
                    constants.repulsionScale
                        / (distance ^ constants.repulsionMagnitude)

                strength =
                    (attraction - repulsion)
                        |> max constants.minForce
            in
            if distance == 0 then
                influence

            else
                clusterPosition
                    |> Vector2.normalize
                    |> Vector2.scale strength
                    |> Vector2.add influence
    in
    surface
        |> Surface.shiftTo entity
        |> Maybe.map
            (Surface.clusters constants.clusteringPrecision
                >> List.foldl addInfluence (vec2 0 0)
            )
