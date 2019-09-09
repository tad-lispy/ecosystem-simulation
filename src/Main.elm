module Main exposing (main)

import Basics.Extra exposing (uncurry)
import Browser
import Browser.Dom as Dom
import Browser.Events
import Circle2d
import Cluster exposing (Cluster)
import Geometry.Interop.LinearAlgebra.Point2d as Point2d
import Geometry.Svg
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import List.Extra as List
import Math.Vector2 as Vector2 exposing (Vec2, vec2)
import Maybe.Extra as Maybe
import Result.Extra as Result
import Svg exposing (Svg)
import Svg.Attributes
import Task exposing (Task)
import Transformation exposing (Transformation)


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
    , paused : Bool
    , area : Vec2
    , scroll : Vec2
    }


type alias Entity =
    Int


universeSize : Float
universeSize =
    1000


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { cluster = Cluster.Empty universeSize
      , paused = True
      , area = vec2 (universeSize + 200) (universeSize + 200)
      , scroll = vec2 -100 -100
      , entities = []
      }
    , Cmd.none
    )


type Msg
    = Animate Float
    | Click Vec2
    | Insert Vec2 (Result Dom.Error Dom.Element)
    | Remove Entity
    | Play
    | Pause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate delta ->
            ( model, Cmd.none )

        Click position ->
            ( model
            , Dom.getElement "scene"
                |> Task.attempt (Insert position)
            )

        Insert mousePosition (Err error) ->
            ( model
            , Cmd.none
            )

        Remove entity ->
            ( { model
                | cluster =
                    Cluster.remove entity model.cluster
              }
            , Cmd.none
            )

        Insert pointer (Ok dom) ->
            let
                entity =
                    List.length model.entities

                position =
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
                    Cluster.insert position entity model.cluster
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



-- Sub.none


view : Model -> Html Msg
view model =
    Html.div []
        [ viewScene
            model.scroll
            model.area
            model.cluster
        , if model.paused then
            Html.button [ Html.Events.onClick Play ] [ Html.text "Play" ]

          else
            Html.button [ Html.Events.onClick Pause ] [ Html.text "Pause" ]
        , Html.button [ Html.Events.onClick (Animate 16) ] [ Html.text "Step" ]
        ]


viewScene : Vec2 -> Vec2 -> Cluster -> Svg Msg
viewScene scroll area cluster =
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
    [ viewCluster (vec2 0 0) cluster
    , viewCursor Nothing
    ]
        |> Svg.svg
            [ Svg.Attributes.width "100%"
            , Svg.Attributes.height "100%"
            , Svg.Attributes.style "background: pink; cursor: crosshair"
            , Svg.Attributes.viewBox viewbox
            , Html.Events.on "click" (mousePositionDecoder Click)
            , Svg.Attributes.id "scene"
            ]


viewCursor : Maybe Vec2 -> Svg Msg
viewCursor cursor =
    case cursor of
        Nothing ->
            Svg.g [] []

        Just position ->
            Svg.circle
                [ Svg.Attributes.fill "black"
                , Svg.Attributes.r "6"
                , position
                    |> Transformation.Translate
                    |> Transformation.toString
                    |> Svg.Attributes.transform
                ]
                []


viewCluster : Vec2 -> Cluster -> Svg Msg
viewCluster translation cluster =
    case cluster of
        Cluster.Empty viewport ->
            Svg.rect
                [ viewport
                    |> String.fromFloat
                    |> Svg.Attributes.width
                , viewport
                    |> String.fromFloat
                    |> Svg.Attributes.height
                , Svg.Attributes.stroke "white"
                , Svg.Attributes.strokeWidth "1"
                , Svg.Attributes.fill "none"
                , translation
                    |> Transformation.Translate
                    |> Transformation.toString
                    |> Svg.Attributes.transform
                ]
                []

        Cluster.Singleton viewport entities position ->
            Svg.rect
                [ viewport
                    |> String.fromFloat
                    |> Svg.Attributes.width
                , viewport
                    |> String.fromFloat
                    |> Svg.Attributes.height
                , Svg.Attributes.stroke "green"
                , Svg.Attributes.strokeWidth "1"
                , Svg.Attributes.fill "none"
                ]
                []
                :: List.map (viewEntity position) entities
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


viewEntity : Vec2 -> Entity -> Svg Msg
viewEntity position entity =
    let
        removeDecoder : Decoder ( Msg, Bool )
        removeDecoder =
            Decode.succeed ( Remove entity, True )
    in
    position
        |> Point2d.fromVec2
        |> Circle2d.withRadius 3
        |> Geometry.Svg.circle2d
            [ Svg.Attributes.fill "red"
            , Svg.Attributes.style "cursor: pointer"
            , Html.Events.stopPropagationOn "click" removeDecoder
            ]
