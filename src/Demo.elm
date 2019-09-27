module Demo exposing (main)

import AltMath.Vector2 exposing (Vec2, vec2)
import Color exposing (Color)
import Ecosystem
    exposing
        ( Duration
        , Group
        , Id
        , Image
        , Interaction
        , Update
        )


main =
    Ecosystem.simulation
        { update = update
        , init = init
        , view = view
        , size = 500
        }


type Entity
    = Pretton
    | Uglon


type alias Action =
    ()


init : List (Update Entity Action)
init =
    [ { this = Just Pretton
      , movement = vec2 200 200
      , interactions = []
      }
    , { this = Just Pretton
      , movement = vec2 -200 200
      , interactions = []
      }
    , { this = Just Uglon
      , movement = vec2 0 -160
      , interactions = []
      }
    ]


update :
    Id
    -> Entity
    -> List (Interaction Action)
    -> List (Group Entity)
    -> Duration
    -> Update Entity Action
update id entity interactions groups duration =
    { this = Just entity
    , interactions = []
    , movement = vec2 0 0
    }


view : Entity -> Image
view entity =
    case entity of
        Uglon ->
            { fill = Color.lightYellow
            , stroke = Color.orange
            , size = 5
            }

        Pretton ->
            { fill = Color.lightBlue
            , stroke = Color.blue
            , size = 5
            }
