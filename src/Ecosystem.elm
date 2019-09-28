module Ecosystem exposing
    ( Duration
    , Group
    , Id
    , Image
    , Interaction
    , Setup
    , Update
    , grid
    , simulation
    )

import AltMath.Vector2 as Vector2 exposing (Vec2, vec2)
import Browser
import Browser.Events
import Color exposing (Color)
import Element exposing (Element)
import Html exposing (Html)
import Html.Attributes
import IntDict exposing (IntDict)
import List.Extra as List
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Transformation exposing (Transformation)
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
                , view = view setup
                , update = update setup
                , subscriptions = subscriptions
                }
    in
    main


type alias Setup entity action =
    { update :
        Duration
        -> Id
        -> entity
        -> List (Interaction action)
        -> List (Group entity)
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
    , paused : Bool
    }


type alias Id =
    Int


type alias Interaction action =
    { other : Id
    , action : action
    }


type alias Group entity =
    -- Alias and re-expose
    WrappedPlane.Group entity


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
            , paused = False
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
                            model.surface
                                |> WrappedPlane.shift movement
                                |> WrappedPlane.place id entity
                                |> WrappedPlane.return
                        , interactions =
                            List.foldl
                                (registerInteraction id)
                                model.interactions
                                interactions
                    }
    in
    ( List.indexedFoldl initReducer empty setup.init
    , Cmd.none
    )


type alias InteractionsRegister action =
    IntDict (List (Interaction action))


type Msg
    = Animate Float
    | Pause


update :
    Setup entity action
    -> Msg
    -> Model entity action
    -> ( Model entity action, Cmd Msg )
update setup msg model =
    case msg of
        Animate duration ->
            let
                updateReducer :
                    Id
                    -> entity
                    -> Model entity action
                    -> Model entity action
                updateReducer id this memo =
                    let
                        incomingInteractions =
                            IntDict.get id model.interactions
                                |> Maybe.withDefault []

                        surface =
                            memo.surface
                                |> WrappedPlane.shiftTo id
                                |> Maybe.withDefault memo.surface
                    in
                    surface
                        |> WrappedPlane.clusters 0.9
                        |> List.filter
                            (\group ->
                                -- FIXME: There is a bug in
                                -- WrappedPlane.clusters and sometimes own
                                -- cluster is reported to position slightly
                                -- different than (0, 0). This works around it.
                                --
                                -- Apart from the bug this prevents extreme
                                -- forces being applied resulting in noisy
                                -- simulations
                                Vector2.lengthSquared group.position > 10
                            )
                        |> setup.update
                            virtualDuration
                            id
                            this
                            incomingInteractions
                        |> (\entityUpdate ->
                                case entityUpdate.this of
                                    Nothing ->
                                        { memo
                                            | surface =
                                                WrappedPlane.remove id memo.surface
                                            , interactions =
                                                IntDict.remove id memo.interactions
                                        }

                                    Just entity ->
                                        { memo
                                            | surface =
                                                surface
                                                    |> WrappedPlane.shift entityUpdate.movement
                                                    |> WrappedPlane.place id entity
                                                    |> WrappedPlane.return
                                            , interactions =
                                                List.foldl
                                                    (registerInteraction id)
                                                    memo.interactions
                                                    entityUpdate.interactions
                                        }
                           )

                virtualDuration : Float
                virtualDuration =
                    -- If the frame rate drops below 30fps then slow down the
                    -- animation but retain precision. Otherwise there is too
                    -- much noise and things get wild.
                    min duration 32
            in
            ( model.surface
                |> WrappedPlane.foldl updateReducer model
            , Cmd.none
            )

        Pause ->
            ( { model | paused = True }
            , Cmd.none
            )


subscriptions : Model entity action -> Sub Msg
subscriptions model =
    if model.paused then
        Sub.none

    else
        Browser.Events.onAnimationFrameDelta Animate


view : Setup entity action -> Model entity action -> Html Msg
view setup model =
    let
        viewport =
            setup.size

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
        |> paintScene setup
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


paintScene : Setup entity action -> Model entity action -> List (Svg Msg)
paintScene setup model =
    model.surface
        |> WrappedPlane.render 500 500
        |> List.map (paintEntity setup)


paintEntity : Setup entity action -> ( Id, entity, Vec2 ) -> Svg Msg
paintEntity setup ( id, entity, position ) =
    let
        image =
            setup.view entity
    in
    Svg.circle
        [ image.size
            |> String.fromFloat
            |> Svg.Attributes.r
        , image.fill
            |> Color.toCssString
            |> Svg.Attributes.fill
        , image.stroke
            |> Color.toCssString
            |> Svg.Attributes.stroke
        , position
            |> Transformation.Translate
            |> Transformation.toString
            |> Svg.Attributes.transform
        ]
        []



-- Helpers


grid :
    Int
    -> Int
    -> Float
    -> (Int -> entity)
    -> List (Update entity action)
grid rows cols distance constructor =
    (rows * cols - 1)
        |> List.range 0
        |> List.map
            (\id ->
                let
                    x =
                        modBy cols id
                            |> toFloat
                            |> (*) distance

                    y =
                        (id // cols)
                            |> toFloat
                            |> (*) distance

                    entity =
                        constructor id
                in
                { this = Just entity
                , movement = vec2 x y
                , interactions = []
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
