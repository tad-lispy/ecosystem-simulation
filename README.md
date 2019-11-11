# Actor Based Ecosystem Simulation Software 

A framework for creating simulations of systems consisting of autonomous actors interacting with each other (e.g. an ecosystem). See [live demos](https://tad-lispy.gitlab.io/ecosystem-simulation/).

## Installation

> NOTE: This is not yet published to packages.elm-lang.org so below instructions won't work. I'm still waiting for community feedback before I publish. If you have any, please share it here (open an issue) or via [Elm Discourse](https://discourse.elm-lang.org/t/actor-based-simulation-framework/4437).
>
> Below is a concept for the future installation procedure. If you want to play with this framework then simply clone the repository and follow the tutorial below. You don't need to install anything.

It's an Elm package. Install it the usual way:

```sh
elm install tad-lispy/ecosystem-simulation
```

For physical calculations (length, velocity, mass, etc.) we are using excellent [Units](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/) and [Geometry](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/) packages by @ianmackenzie and to control colors the [Color](https://package.elm-lang.org/packages/avh4/elm-color/latest/) package by @avh4. You will need them too:

```sh
elm install ianmackenzie/elm-units
elm install ianmackenzie/elm-geometry
elm install avh4/elm-color
```


## Tutorial

> I think it's good to explain this framework by walking you through an example simulation. This tutorial is written with an Elm beginner (maybe even a non-programmer) in mind. If you would rather jump straight into API docs then head to the package page (TODO: not ready yet, sorry). 

Let's build a simple simulations. Start by [installing Elm](https://guide.elm-lang.org/install.html) and setting up a project:

```sh
elm init
```

Then install this package and other dependencies as described [above in the Installation section](#installation and create `src/Main.elm` with the following code inside:

> TODO: Once the package is published we can use Ellie for this tutorial.

### First Simulation Program

<!-- TODO: Use a template to generate README.md

Maybe m4? See https://mbreen.com/m4.html#toc21

This should allow to include the actual code from src/Demos/Empty.elm here.

Workflo could be the following. There is a README.m4 file (editable) and README.md (generated). We could use command like

  make readme

To generate it. In the CI the pipeline should fail if running the above command results in any changes (git diff trick). The readme target should be a dependency of the all target. If we also have a demos target, then the correctness of examples should be sufficently safeguarded.
-->
```elm
module Main exposing (main)

import Color 
import Ecosystem exposing (Change(..))
import Environment 
import Length exposing (meters)
import Vector2d 


main =
    Ecosystem.simulation
        { size = meters 500
        , updateActor = updateActor
        , paintActor = paintActor
        , init = init
        }


init =
    []


updateActor id this environment =
    { velocity = Vector2d.zero
    , change = Unchanged
    , spawn = []
    , interactions = []
    }


paintActor actor =
    { size = meters 2
    , fill = Color.lightBlue
    , stroke = Color.blue
    }
```

This should be enough to compile our program. Later I will discuss the code above, but first let's just try to run it. Enter the following command in the terminal:

```sh
elm reactor
```

In your browser open http://localhost:8000/src/Main.elm - you should see a perfectly empty screen. That's ok since we don't have any actors in our system yet. So, what is the meaning of all of this?

### The Setup

First let's look at the `main` value. It's a standard Elm program. You can read more about it in [the Official Elm Guide](https://guide.elm-lang.org/). We construct it by calling the `Ecosystem.simulation` function and passing it a record of type `Ecosystem.Setup actor action`. We will get to the `actor` and the `action` type parameters later.

> The types are very important concept in Elm and sometimes I explain things in terms of types. Usually they are uppercase words (Like `Setup`, `Length`, etc). If I give type annotations (like below) it goes like this: first the name of the value (e.g. `size`) then a colon (`:`) and then type (e.g. `Length`). Don't wory if you don't understand the type system very well yet. You should be able to follow the tutorial anyway. Just glance over the fragments about types and come back later when you are more familiar with the concept.

The setup record consists of four fields:

- `size : Length`

  The simulation takes place on a square wrapped plane. This field defines how long is the edge of this square. Basically how big is the simulated world. To express the size we need the `Length` module imported on top of our file and its `meters` function - to get length in meters.

- `init : List (Spawn actor action)`

  The initial setup of the simulation. We will discuss the `Spawn actor action` type in a moment. The number of elements in this list will translate to number of actors present at the beginning of the simulation. Later these actors can spawn new actors or remove themselves, but we need to start the whole show somehow. Since the list is empty, there are no actors - hence the empty screen.

- `updateActor : Id -> actor -> Environment actor action -> Update actor action`

  This function will be called for each actor on every frame of the simulation. It will be passed three arguments:

  1.  The id of this actor

  2.  The state of this actor 

  2.  The data about the environment 

      We can use this data to get information about the environment such as:

      - other actors and their positions relative to this one,

      - interactions concerning this actor,

      - how much time has passed since last update 

        so we can calculate rates of changes, like velocity etc.). 

      It will be discussed in details later.

  The `updateActor` function must return a record of type `ActorUpdate actor action` with the following fields:

  - `change : Change actor`
  - `velocity : Vector2d MetersPerSecond Coordinates`
  - `interactions : List (Interaction action)`
  - `spawn : List (Spawn actor action)`

  We will discuss it in a moment (I promise!)

- Paint Actor : actor -> Image

  How should your actor be represented on the screen. Image is a record:

  ```elm
  { size : Length -- the radius of the dot
  , fill : Color -- color of the inside
  , stroke : Color -- color of the outline (outer 20%)
  }
  ```

### First Actors

Ok, so what is this whole `Spawn actor action` type? It's a record:

```elm
{ actor : actor
, displacement : Vector2d Meters Coordinates
, interactions : List (Interaction action)
}
```

The `actor` field controls what is the initial state of the new actor. We will need to come up with some type of value that will describe our actors.

The `displacement` controls how far and in what direction the new actor will spawn relative to its parent. The initial actors have no parents, so they will spawn in an arbitrary point (let's call it the origin). You can use displacement to move them away from this point, so that if you have more than one actor, they don't end up all in one place.

Newly spawned actors can immediately interact with other actors. You could use the `interactions` field for that, but for now let's not do this. It's a list, so we can simply set it to `[]` - an empty list.

We have setup some concepts and should be ready to add some actors to our simulation. First we will need to provide concrete types to actor and action type parameters that we saw everywhere. Let's define them. The simplest type in Elm is called unit. It looks like this: `()`. There is only one possible value of this type that is also called unit and looks like this: `()`. Seriously. So let's define our custom types. Put the following in your code right after the all the imports:

```elm
type alias Actor =
    ()

type alias Action =
    ()
```

Now we can plug these types in to our program:

```elm
init =
    [ { actor = ()
      , displacement = Vector2d.zero
      , interactions = []
      }
    ]
```

That should be enough to produce a single actor - although without much agency yet. Reload the browser and observe a single motionless blue dot in the middle of the screen.

Let's place another actor five meters to the west of the first one.

```elm
init =
  [ ... 
    , { actor = ()
      , displacement = Vector2d.meters -5 0
      , interactions = []
      }
  ]
```

> The `...` means that you should just leave the same code as there was before - don't type it literally. 

### Let Them Move!

And now we have two perfectly still actors. Let's give them some agency starting with movement. We can do it using the velocity field of the `ActorUpdate Actor Action` record. Velocity is a single value expressing speed and direction. We will need to decide on both of them. 

Let's choose a speed then. We can pick any value we want here. I like when things are moving fast - let's say 5 m/s. Then a direction. Again, anything will work, but we got to choose something. So let them move left. In terms of our coordinate systen it will be `Direction2d.positiveX`. Here is how we need to change the `updateActor` function to express this: 

```elm
updateActor id this environment =
    let
        speed =
            metersPerSecond 5

        velocity =
            Vector2d.withLength speed Direction2d.positiveX
    in
    { velocity = velocity
    , change = Unchanged
    , spawn = []
    , interactions = []
    }
```

To make this work we need to import the `Speed` and `Direction2d` modules. Add these lines where the rest of the imports are (around line 3):

```elm
import Speed exposing (metersPerSecond)
import Direction2d
```

Reload the browser and your actors should march across the screen at a steady pace of 5m per second. By this I mean 5m in their world - on your screen it will be much smaller distance. Everything is scaled. Notice that once they reach the "edge" they will re-appear on the other side. That's what I'm talking about when saying that the simulation happens on a wrapped plane. It works the same for left-right and up-down movement. In fact for the actors there is no such thing as the edge of the world - just like there is no such thing as the edge of the earth for us (take that, flat-earthers 🌍)

### The ActorUpdate Record

Time to discuss the `ActorUpdate actor action` type. It is what we are returning from `updateActor` function on every frame of the simulation. It's a record that describes what happens to each actor. Here is its type:

```elm
{ velocity : Vector2d MetersPerSecond Coordinates
, change : Change actor
, interactions : List (Interaction actor)
, spawns : List (Update actor action)
}
```

The `velocity` field controls in which direction and how fast is the actor moving in this frame. 

> Don't worry about the `Coordinates` type parameter - if you ever need to provide it just use `Ecosystem.Coordinates`.

The `change` controls how the internal state of the actor changes. It's a union type with the following constructors:

  - `Unchanged`

    the actor remains as it was

  - `Changed actor`

    the actor changes. Its new state is the tagged value of type `actor` so in our case it can only be `()`. No way to change really. But later we will see how actors can change their state or even morph into completely different kinds of actors (e.g. from `Fly { annoyence: Float }` into `SpotOnTheWall { howDifficultWillItBeToWipeIt: Float }`. 

  - `Removed` 

    the actor is to be removed from the simulation.

The `interactions` field describes all actions taken by this actor towards other actors. We will cover interactions later. For now we will just assign an empty list here i.e there will be no interactions.

The `spawns` field allows an actor to create new actors. It's a List of `Spawn actor action` values that I described [above when discussing the `init` value](#first-actors). 

### The Challenge

I hope you didn't find it too difficult so far, but also let's agree that it's not a very interesting simulation yet. Our actors are pretty dumb and stubborn. How about giving them the following behaviours:

- They will move away from each other, as if disgusted by one another.

- If there is no other actor in 50m radius, they will spawn a single, equally disgusting child.

Sounds like fun? Let's analyse the problem. First the movement.

#### The Movement

Actor needs to know where is the nearest other actor. Fortunately this information is provided to the `updateActor` function as the third argument called the `environment`. We can get positions of other actors by calling the `Environment.actors` funtion with the `environment` value. 

> It has a following signature:
> 
> ```
> Environment.actors : 
>     Environment actor action 
>     -> List Group
> ```
> 
> We can read it like this: `Environment.actors` is a function that takes one argument of type `Environment actor action` (a type with two parameters, in our case it's `Environment Actor Action` which is the same as `Environment () ()`). When given this argument it will return a value of type `List Group` - list of groups. 

It will give us a list of all other actors grouped into clusters. Each group comes with a position relative to this actor (the one who calls the function) and a list of its members (`List Ecosystem.Id`). Here we are only interested in the position of the nearest group. We can get it by sorting the list by distance and taking the first element (the head of the list):

```elm
nearest = 
    environment
        |> Environment.actors
        |> List.sortBy
            (.position
                >> Vector2d.length
                >> Length.inMeters
            )
        |> List.head
```

> The `|>` is the function application operator sometimes called the pipe or the pizza operator. The `>>` is the function composition operator. You can read more about them in [the official Elm documentation](https://elm-lang.org/docs/syntax#operators).

Once we have the result as `nearest` then we can define `direction` as:

```elm
direction =
    nearest
        |> Maybe.map .position
        |> Maybe.andThen Vector2d.direction
        |> Maybe.map Direction2d.reverse
```

We already defined a speed before, so we should have everything to re-define the velocity - speed together with direction:

```elm
velocity =
    direction
        |> Maybe.map (Vector2d.withLength speed)
        |> Maybe.withDefault Vector2d.zero
```

> What with all the `Maybe`s?
> 
> First it may be that there are no other actors in this simulation. Then the list of groups would be empty and its head would be `Nothing`. So `nearest` is a `Maybe Group`.
> 
> Second maybe concerns the direction. If the other actor is exactly in the same spot as this one then there would be no sense to talk about a direction (what's the direction from here to here?) and the `Vector2d.direction` function would return `Nothing`. Truth be told it will never happen - if another actor is at the same point as this one it will be skipped by the `Environment.groups` function. But the type system doesn't understands this. So `Vector2d.direction` returns a `Maybe Direction2d` and we need to deal with it. We combine the two maybes into one using clever `Maybe.andThen` function. If either of them is `Nothing` then the actor won't move (the `velocity` is set to `Vector2d.zero`). If both the nearest group and the direction exists then we use them to calculate velocity. 

That should solve the movement issue. Let's plug it into the `let` block and see. The whole `updateActor` function should look like this:

```elm
updateActor id this environment =
    let
        speed =
            metersPerSecond 5

        nearest =
            environment
                |> Environment.actors
                |> List.sortBy
                    (.position
                        >> Vector2d.length
                        >> Length.inMeters
                    )
                |> List.head

        direction =
            nearest
                |> Maybe.map .position
                |> Maybe.andThen Vector2d.direction
                |> Maybe.map Direction2d.reverse

        velocity =
            direction
                |> Maybe.map (Vector2d.withLength speed)
                |> Maybe.withDefault Vector2d.zero
    in
    { change = Unchanged
    , velocity = velocity
    , interactions = []
    , spawn = []
    }
```

In the browser we should observe that the two actors move away from each other until they are half the world apart. At this stage they start to shake. So strong is their mutual repulsion that they go to the opposite sides of their world and still try to go further away. Unfortunately every step brings them closer together, so immediately they take a step back. Hell is other actors.

> Sometimes they start chasing each other around the world in a kind of lockstep.

#### Actors Making New Actors

Ok, now what about spawning? Let's consider where should an actor place its beloved child. Of course away from those nasty other actors. Simplest thing is to spawn away from the nearest group. 

But wait! What if there is no other group (`nearest` is `Nothing`)?  If there is no "others" then there is no "away". We need to make some design decisions. We could just decide that the actor should remain the only one in the simulation. Why spoil the perfect state of loneliness. But that would be rather boring for us to watch. So why not spawn four nasty little actors and let them run away in all directions? Let's do that!

In the let block define `spawn` as follows.

```elm
case direction of
    Nothing ->
        [ { actor = this
          , displacement = Vector2d.meters 3 0
          , interactions = []
          }
        , { actor = this
          , displacement = Vector2d.meters 0 3
          , interactions = []
          }
        , { actor = this
          , displacement = Vector2d.meters -3 0
          , interactions = []
          }
        , { actor = this
          , displacement = Vector2d.meters 0 -3
          , interactions = []
          }
        ]
```

Now we need to handle the case where there are other actors. We only want to spawn if they are further away than 50m. So we need to know the distance to the nearest group. We can do it like this:

```elm
case direction of 
    Nothing ->
        ...

    Just away ->
        if
            nearest
                |> Maybe.map .position
                |> Maybe.withDefault Vector2d.zero
                |> Vector2d.length
                |> Quantity.greaterThan (Length.meters 50)
        then
            [ { actor = this
              , displacement =
                    Vector2d.withLength
                        (Length.meters 2)
                        away
              , interactions = []
              }
            ]

        else
            []
```

> Again we had to deal with a maybe. Remember that `nearest` is a `Maybe Group`.

We have to add new import:

```elm
import Quantity
```

Here is the complete code for the demo program:

```elm
module Main exposing (main)

import Color
import Direction2d
import Ecosystem exposing (Change(..))
import Environment
import Length exposing (meters)
import Quantity
import Speed exposing (metersPerSecond)
import Vector2d


main : Ecosystem.Program Actor Action
main =
    Ecosystem.simulation
        { size = meters 500
        , updateActor = updateActor
        , paintActor = paintActor
        , init = init
        }


type alias Actor =
    ()


type alias Action =
    ()


init =
    [ { actor = ()
      , displacement = Vector2d.zero
      , interactions = []
      }
    , { actor = ()
      , displacement = Vector2d.meters -5 0
      , interactions = []
      }
    ]


updateActor id this environment =
    let
        speed =
            metersPerSecond 5

        nearest =
            environment
                |> Environment.actors
                |> List.sortBy
                    (.position
                        >> Vector2d.length
                        >> Length.inMeters
                    )
                |> List.head

        direction =
            nearest
                |> Maybe.map .position
                |> Maybe.andThen Vector2d.direction
                |> Maybe.map Direction2d.reverse

        velocity =
            direction
                |> Maybe.map (Vector2d.withLength speed)
                |> Maybe.withDefault Vector2d.zero

        spawn =
            case direction of
                Nothing ->
                    [ { actor = this
                      , displacement = Vector2d.meters 3 0
                      , interactions = []
                      }
                    , { actor = this
                      , displacement = Vector2d.meters 0 3
                      , interactions = []
                      }
                    , { actor = this
                      , displacement = Vector2d.meters -3 0
                      , interactions = []
                      }
                    , { actor = this
                      , displacement = Vector2d.meters 0 -3
                      , interactions = []
                      }
                    ]

                Just away ->
                    if
                        nearest
                            |> Maybe.map .position
                            |> Maybe.withDefault Vector2d.zero
                            |> Vector2d.length
                            |> Quantity.greaterThan (meters 50)
                    then
                        [ { actor = this
                          , displacement =
                                Vector2d.withLength
                                    (meters 2)
                                    away
                          , interactions = []
                          }
                        ]

                    else
                        []
    in
    { change = Unchanged
    , velocity = velocity
    , interactions = []
    , spawn = spawn
    }


paintActor actor =
    { size = meters 1
    , fill = Color.white
    , stroke = Color.green
    }
```

Let's reload the browser and see! Hopefully they behave the way we wanted them to. And if we wait long enough the actors will fill the space more or less evenly and stop reproducing. That's interestingly because we didn't directly program them to do so. We have just instilled a deeply rooted hatred to one another and strong urge to reproduce. And here they are conquerring the world and exploiting every last bit of it. Maybe there is a lesson here? 

This is what I would call an emerging property. We program micro behaviours of actors and observe macro trends in the system.

If you want to share your simulation, run the following command in the terminal:

```sh
elm make Main.elm
```

It will create a file called `index.html` that you can open in your browser, send to a friend or publish on-line.

I hope you didn't find it too difficult to follow so far. If something in this tutorial is not clear or just wrong please reach out to me by opening an issue or via Elm slack. I'm Tad Lispy there too. 

> TODO: Next we will introduce a second kind of actor with a different attitude and play with interactions.

## Design Goals

There are several goals and constraints that drive the development of this system. I discuss them below.

### Fun 😀

This system was created mostly for fun and I hope it will remain fun to use it for simulations development.

### Simplicity

I hope it can be used by hobbysts (for fun) or in education (programming, ecology, systems thinking). This means that it should be easy to set up. Preferabely development should not require anything else than Elm compiler (and tools coming with it) or a service like [Ellie](https://ellie-app.com/) and a modern web browser. Results should be easy to share (publish on-line, etc.)

### Actor centric development 

Applications should focus on programming actors. The macro behaviour of a system should be an amerging property of micro behaviours of actors. We program behaviours and watch the trends emerging.

### Esthetics

Smooth animations, nice colors, crispy shapes. It should be attractive and fun to play with. Let's try to keep kids interested in it long enough for them to learn something.

## Ideas for Further Development

- Richer environment
  
  Global state of the ecosystem (e.g. climate) and local conditions of the environment (e.g. weather).

- Statistics

  In the app there should be some simple graphs (population, etc.) There should also be a way to export data for more advanced analytics with tools like Jupyter Notebook.

- Embedding simulations

  A way to run and control the simulation as part of your own Elm application.
