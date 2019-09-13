module Main exposing (main)

import AltMath.Vector2 as Vector2 exposing (Vec2, vec2)
import Basics.Extra exposing (uncurry)
import Browser
import Browser.Dom as Dom
import Browser.Events
import Circle2d
import Cluster exposing (Cluster)
import Geometry.Svg
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import List.Extra as List
import Maybe.Extra as Maybe
import Point2d
import Result.Extra as Result
import Svg exposing (Svg)
import Svg.Attributes
import Task exposing (Task)
import Transformation exposing (Transformation)


constants =
    { universeSize = 3000
    , entityMass = 0.01
    , minForce = -0.001
    , repulsionMagnitude = 1.5
    , repulsionScale = 100
    , attractionMagnitude = 1
    , attractionScale = 1
    , maxDelta = 32
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
    { cluster : Cluster
    , entities : List Entity
    , selected : Maybe Entity
    , paused : Bool
    , area : Vec2
    , scroll : Vec2
    }


type alias Entity =
    Int


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        ( entities, cluster ) =
            grid 20 20 100
    in
    ( { cluster = cluster
      , paused = True
      , selected = Nothing
      , area = vec2 (constants.universeSize + 200) (constants.universeSize + 200)
      , scroll = vec2 -100 -100
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
                cluster =
                    forces
                        |> List.foldl
                            (\( entity, f ) memo ->
                                case Cluster.location entity memo of
                                    Nothing ->
                                        memo

                                    Just location ->
                                        memo
                                            |> Cluster.remove entity
                                            |> Cluster.insert
                                                (Vector2.add location
                                                    (Vector2.scale
                                                        (virtualDelta / constants.entityMass)
                                                        f
                                                    )
                                                )
                                                entity
                            )
                            model.cluster

                forces =
                    model.entities
                        |> List.foldl
                            (\entity memo ->
                                case force model.cluster entity of
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
                | cluster = cluster
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
                        |> Vector2.add model.scroll

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
                , cluster =
                    Cluster.insert location entity model.cluster
              }
            , Cmd.none
            )

        Remove entity ->
            ( { model
                | cluster =
                    Cluster.remove entity model.cluster
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
    Html.div []
        [ if model.paused then
            Html.button [ Html.Events.onClick Play ] [ Html.text "Play" ]

          else
            Html.button [ Html.Events.onClick Pause ] [ Html.text "Pause" ]
        , Html.button [ Html.Events.onClick (Animate 16) ] [ Html.text "Step" ]
        , viewScene
            model.scroll
            model.area
            model.selected
            model.cluster
        ]


viewScene :
    Vec2
    -> Vec2
    -> Maybe Entity
    -> Cluster
    -> Svg Msg
viewScene scroll area selected cluster =
    let
        mousePositionDecoder : (Vec2 -> Msg) -> Decoder Msg
        mousePositionDecoder callback =
            Decode.map2
                vec2
                (Decode.field "clientX" Decode.float)
                (Decode.field "clientY" Decode.float)
                |> Decode.map callback

        viewbox =
            [ Vector2.getX scroll
            , Vector2.getY scroll
            , Vector2.getX area
            , Vector2.getY area
            ]
                |> List.map String.fromFloat
                |> String.join " "
    in
    [ viewSelection selected cluster
    , viewCluster (vec2 0 0) cluster
    ]
        |> Svg.svg
            [ Svg.Attributes.width "100%"
            , Svg.Attributes.height "100%"
            , Svg.Attributes.style "background: black; cursor: crosshair"
            , Svg.Attributes.viewBox viewbox
            , Html.Events.on "click" (mousePositionDecoder Click)
            , Svg.Attributes.id "scene"
            ]


viewCluster : Vec2 -> Cluster -> Svg Msg
viewCluster translation cluster =
    case cluster of
        Cluster.Empty _ ->
            Svg.g [] []

        Cluster.Singleton _ entities location ->
            entities
                |> List.map (viewEntity location)
                |> Svg.g
                    [ translation
                        |> Transformation.Translate
                        |> Transformation.toString
                        |> Svg.Attributes.transform
                    ]

        Cluster.Cluster viewport _ subClusters ->
            [ subClusters.topLeft
                |> viewCluster (vec2 0 0)
            , subClusters.topRight
                |> viewCluster (vec2 (viewport / 2) 0)
            , subClusters.bottomLeft
                |> viewCluster (vec2 0 (viewport / 2))
            , subClusters.bottomRight
                |> viewCluster (vec2 (viewport / 2) (viewport / 2))
            ]
                |> Svg.g
                    [ translation
                        |> Transformation.Translate
                        |> Transformation.toString
                        |> Svg.Attributes.transform
                    ]


viewSelection : Maybe Entity -> Cluster -> Svg Msg
viewSelection selected cluster =
    case selected of
        Nothing ->
            Svg.g [] []

        Just entity ->
            case Cluster.location entity cluster of
                Nothing ->
                    Svg.g [] []

                Just location ->
                    cluster
                        |> Cluster.clusters 0.9 location
                        |> List.map
                            (Tuple.mapFirst
                                (\otherLocation ->
                                    Vector2.sub otherLocation location
                                )
                            )
                        |> List.map viewRelation
                        |> Svg.g
                            [ location
                                |> Transformation.Translate
                                |> Transformation.toString
                                |> Svg.Attributes.transform
                            ]


viewEntity : Vec2 -> Entity -> Svg Msg
viewEntity location entity =
    let
        removeDecoder : Decoder ( Msg, Bool )
        removeDecoder =
            Decode.succeed ( Remove entity, True )
    in
    ( Vector2.getX location
    , Vector2.getY location
    )
        |> Point2d.fromCoordinates
        |> Circle2d.withRadius 3
        |> Geometry.Svg.circle2d
            [ Svg.Attributes.fill "red"
            , Svg.Attributes.style "cursor: pointer"
            , Html.Events.stopPropagationOn "click" removeDecoder
            , Html.Events.onMouseEnter (Select <| Just entity)
            , Html.Events.onMouseLeave (Select Nothing)
            ]


viewRelation : ( Vec2, List Entity ) -> Svg Msg
viewRelation ( vector, entities ) =
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


grid : Int -> Int -> Float -> ( List Int, Cluster )
grid rows cols distance =
    let
        width =
            distance * toFloat rows

        height =
            distance * toFloat rows

        xOffset =
            (constants.universeSize - width) / 2

        yOffset =
            (constants.universeSize - height) / 2

        entities =
            List.range 0 (rows * cols - 1)

        cluster =
            entities
                |> List.foldl
                    (\entity memo ->
                        let
                            x =
                                modBy cols entity
                                    |> toFloat
                                    |> (*) distance
                                    |> (+) xOffset

                            y =
                                (entity // cols)
                                    |> toFloat
                                    |> (*) distance
                                    |> (+) yOffset
                        in
                        Cluster.insert (vec2 x y) entity memo
                    )
                    (Cluster.Empty constants.universeSize)
    in
    ( entities, cluster )


force : Cluster -> Entity -> Maybe Vec2
force cluster entity =
    let
        addInfluence : Vec2 -> ( Vec2, List Entity ) -> Vec2 -> Vec2
        addInfluence location ( clusterLocation, clusterEntities ) influence =
            let
                distance =
                    Vector2.distanceSquared
                        location
                        clusterLocation

                clusterCharge =
                    clusterEntities
                        |> List.length
                        |> toFloat

                attraction =
                    (constants.attractionScale * clusterCharge)
                        / (distance ^ constants.attractionMagnitude)

                repulsion =
                    (constants.repulsionScale * clusterCharge)
                        / (distance ^ constants.repulsionMagnitude)

                strength =
                    (attraction - repulsion)
                        |> max constants.minForce
            in
            if distance == 0 then
                influence

            else
                Vector2.sub clusterLocation location
                    |> Vector2.normalize
                    |> Vector2.scale strength
                    |> Vector2.add influence
    in
    cluster
        |> Cluster.location entity
        |> Maybe.map
            (\location ->
                cluster
                    |> Cluster.clusters 0.9 location
                    |> List.foldl
                        (addInfluence location)
                        (vec2 0 0)
            )
