# Actor Based Simulation Kit 

A framework for creating simulations of systems consisting of autonomous actors interacting with each other (e.g. an ecosystem).

## Installation

It's an Elm package. Install it the usual way:

```sh
elm install tad-lispy/elm-actors-simulation
```

For calculations we are using excelent [Units](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/) and [Geometry](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/) packages by @ianmackenzie and to control colors the [Color](https://package.elm-lang.org/packages/avh4/elm-color/latest/) package by @avh4. You will need them too:

```sh
elm instell ianmackenzie/elm-units
elm instell ianmackenzie/elm-geometry
elm instell avh4/elm-color
```


## Tutorial

> I think it's easiest to explain by showing an example. It is written with an Elm beginner (maybe even a non-programmer). If you would rather jump straight into API docs then head to the package page (TODO: not there yet, sorry). 

Let's build a simple simulations. Start by [installing Elm](https://guide.elm-lang.org/install.html) and setting up a project:

```sh
elm init
```

Then install this package and other dependencies as described [above in the Installation section](#installation). Now create `src/Main.elm` with the following code inside:

### First Simulation Program

```elm
module Main exposing (main)

import Ecosystem exposing (ActorUpdate, Change(..), Id, Spawn)
import Color exposing (Color)
import Length exposing (Length, meters)

main =
      Ecosystem.simulation
              { size = meters 500
              , updateActor = updateActor
              , paintActor = paintActor
              , init = init
              }


init =
      []


updateActor environment id =
    { movement = Vector2d.zero
    , self = Unchanged
    , spawn = []
    , interactions = []
    }


paint actor =
    { size = meters 0.2
    , fill = Colors.lightBlue
    , stroke = Colors.blue
    }
```

This should be enough to compile our program. Later I will discuss the code above, but first let's just try to run it. Enter the following command in the terminal:

```sh
elm reactor
```

In your browser open http://localhost:8000/src/Main.elm - you should see a perfectly empty screen. That's ok since we don't have any actors in our system yet. So, what is the meaning of all of this?

### Setup Record

First our `main` value. It's a standard Elm platform program. You can read more about it in the Official Elm Guide. We construct it by calling the Ecosystem.simulation function and passing it `Ecosystem.Setup actor action` record. We will get to `actor` and `action` type parameters later.

The setup record consists of four fields:

- `size : Length`

  The simulation takes place on a square wrapped plane. This field defines the length of the edge of this square. Basically how big is the simulated world. To express the size we need the `Length` module imported on top of our file and its `meters` function - to get lenght in meters.

- `init : List (Spawn actor action)`

  The initial setup of the simulation. We will discuss the `Spawn actor action` type in a moment. The number of elements in this list will translate to number of actors present at the beginning of the simulation. Later this actors can spawn new actors or remove themselves, but we need to start the whole show somehow. Since the list is empty, there are no actors - hence the empty screen.

- `updateActor : Environment actor action -> Id -> Update actor action`

  This function will be called for each actor on every frame of the animation. It will be passed two arguments:

  1.  The `Id` of this actor

  2.  The `Environment actor action` 
  
      We can use this argument to get information about the environment, such as other actors and their positions relative to this one, interactions concerning this actor, how much time has passed since last update (so we can calculate rates of changes, like velocity etc.). It will be discussed in details later.

  The `updateActor` function must return an `ActorUpdate actor action` record with the following fields:

  - `self : Change actor`
  - `movement : Vector2d Meters Coordinates`
  - `interactions : List (Interaction action`
  - `spawn : List (Spawn actor action)`

  We will discuss it in a moment (I promise!).

- Paint Actor : actor -> Image

  How should your actor be represented on the screen. Image is a record:

  ```elm
  { size : Length -- the radius of the dot
  , fill : Color -- color of the inside
  , stroke : Color -- color of the outline (outer 20%)
  }
  ```

### Spawn Record

Ok, so what id this whole `Spawn actor action` type? It's a record :

```elm
{ actor : actor
, displacement : Vector2d Meters Coordinates
, interactions : List (Interaction action)
}
```

The `actor` field controls what is the initial state of the new actor. We will need to come up with some type of value that will describe our actors.

The `displacement` controls how far and in what direction the new actor will spawn relative to its parent. The initial actors have no parents, so they will spawn in an arbitrary point (lets call it the origin). You can use displacement to move them away from this point, so that if you have more than one actor, they don't end up all in one place.

Newly spawned actors can immediately interact with other actors. You could use the `interactions` field for that, but for now let's not do this. It's a list, so we can simply set it to `[]` - no interactions.

### First Actors

We have setup some concepts and should be ready to add some actors to our simulation. First we will need to provide concrete types to actor and action type parameters that we saw everywhere. Let's define them. The simplest type in Elm is called unit. It looks like this: `()` and can have only one value, that is also called unit and looks like this: `()`. Seriously.

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
      , displacement = Vector.zero
      , interactions = []
      }
    ]
```

That should be enough to produce a single actor - although without much agency yet. Reload the browser and observe a single motionless blue dot in the middle of the screen.  

Let's add another one five meters to the north.

```elm
init =
  [ ... 
  , Update
      { movement = Vector2d.meters 5 0
      , ...
      }
  ]
```

The `...` means that you should just leave the same code as there was before - don't type it literally. And now we have two perfectly still colors. Let's give them some agency. For this we need to change the `updateActor` function. For starters let's make the actors go east with a speed of 0,5 m/s:

### Let Them Move!

```elm
updateActor environment id =
    let 
        duration = 
            Ecosystem.latency environment

        speed =
            Speed.metersPerSecond 0.5

        distance =
            Quantity.for duration speed

        movement =
            Vector2d.meters distance 0
    in
    { movement = movement
      ...
    }
```

Reload the browser and your actors should march across the screen at a steady pace of 50cm per second. By this I mean 50cm in their world - on your screen it will be much smaller distance. Everything is scaled. Notice that once they reach the "edge" they will re-appear on the other side. That's what I mean by wrapped plane. It works the same for left-right and up-down movement. In fact for the actors there is no such thing as the edge of the world - just like there is no such thing as the edge of the earth for us (take that, flat-earthers 🌍)

### ActorUpdate Record

Ok, so what id this whole `ActorUpdate actor action` type that we are returning from `updateActor` function? It's a record :

```elm
{ movement : Vector2d
, change : Change actor
, interactions : List (Interaction actor)
, spawns : List (Update actor action)
}
```

The `movement` field controls in which direction and how far did the actor move in this frame. You will most likely want to take letency (i.e. time since last frame) into account.

> Idea: maybe movement should become `velocity`? It should be easier for the application developer that way.

The `change` controls how the internal state of the actor changes. It's a union type with the following constructors:

  - `Unchanged`  

    the actor remains as it was

  - `Changed actor`

    the actor changes. Its new state is the tagged value of type `actor`. So in our case it can only be `()` - no way to change really. But later we will see how actors can change their state or even morph into completely different kinds of actors (e.g. from `Fly { annoyence: Float }` into `SpotOnTheWall { howDifficultWillItBeToWipe: Float }`. 

  - `Removed` 

    the actor is to be removed from the simulation.

The `interactions` field describes all actions taken by this actor towards other actors. We will cover interactions later, for now we will just assign an empty list here, i.e there will be no interactions.

The `spawns` field allows the actor to create new actors. It's a List of `Spawn actor action` values that I described above when discussing the `init` value. 

### A Challenge

I hope you didn't find it too difficult so far, but also let's agree that it's not a very interesting yet simulation yet. Our actors are pretty dumb and stubborn. How about giving them the following behaviours:

- They will move away from each other, as if disgusted by one another.

- If there is no other actor in 50m radius, they will spawn a single, equally disgusting child.

Sounds like fun. Let's analyse the problem. First the movement.

Actor needs to know where is the nearest other actor. Fortunately this information is provided to the `updateActor` function as the first argument. We can get it by calling `Environment.actors : Environment actor action -> List Group`. It will give use a list of all other actors grouped into clusters. Each group comes with a position relative to this actor (the one who calls the function) and a list of its members. We are only interested in the nearest group. We can get it by sorting the list by distance and taking the first element if any (the head) :

```elm
    let nearest =
        environment
            |> Environment.actors
            |> List.sortBy (.position >> Vector2d.length)
            |> List.head
```

If we have the result as `nearest` then we can define `movement` as :

```elm
movement =
    case nearest of
          Nothing ->
              -- I am the only actor here!
              Vector.zero
          Just group ->  
              let
                  direction = 
                      group.position
                          |> Vector2d.reverse
                          |> Vector2d.direction
              in
                  direction 
                      |> Maybe.map (Vector2d.withLength direction distance)
                      |> Maybe.withDefault (Vector2d.zero)
                      |> Vector2d.for duration
```

What with all the `Maybe`s?

First it may be that there are no other actors in this simulation. Then List of groups would be empty and its head would be `Nothing`.

Second maybe concerns the direction. Truth be told it should never happen - if the other actor is exactly in the same spot as this one it would be omitted in by `Environment.groups`. But the type system doesn't know about it so it theoretically can happen that another actor is at the same point as this one. In that case there would be no sense to talk about a direction and `Vector2d.direction` would return `Nothing`. So in that case we don't move either (`Vector2d.zero`).

That should solve the movement issue. Let's plug it into the `let ... in` block and see. In the browser we should observe that the two actors move away from each other until they are half the world apart. At this stage they start to shake. So strong is their mutual repulsion that they go to the opposite sides of their world and still try to go further away. Unfortunately every step brings them closer together, so immediately they take a step back. Hell is other actors.

Ok, now what about spawning. First we need to know the distance to the nearest group. But wait! What if there is no other group (`nearest` is `Nothing`)? Let's also consider where to place our beloved child. Of course away from those nasty other actors. Only if there is no other's then there is no "away" direction. We need to make some design decisions.

We could just decide that the actor should remain the only one in the simulation. Why spoil the perfect state of loneliness. But that would be rather boring for us to watch.

So why not spawn four nasty little actors and let them run away in all directions? Let's do that!

```elm
spawns =
    case nearest of
        Nothing ->
            [ { actor = ()
              , displacement = Vector2d.with length
                  Directio2d.positiveX (meters 0.5)
              , interactions = []
              }
            , { actor = ()
              , displacement = Vector2d.with length
                  Directio2d.positiveY (meters 0.5)
              , interactions = []
              }
            , { actor = ()
              , displacement = Vector2d.with length
                  Directio2d.negativeX (meters 0.5)
              , interactions = []
              }
            , { actor = ()
              , displacement = Vector2d.with length
                  Directio2d.negativeY (meters 0.5)
              , interactions = []
              }
            ]

        Just group ->
            case Vector2d.direction group.position of
                Nothing ->
                    [] -- this should never happen, but we need to satisfy the type system.

                Just direction ->
                    [ { displacement = 
                          Vector2d.withLength (Directio2d.reverse direction) (meters 0.5)
                      , ...
                      }
                    ]
```

This should do the trick. Lets see! Now notice that if you wait long enough the actors should fill the space more or less evenly and stop reproducing. Interestingly we didn't really program them to do so. We have just instilled a deeply rooted hatred to one another and strong urge to reproduce. And here they are conquerring the world and exploiting every last bit of it. Maybe there is a lesson here? This is what I would call an emerging property. We program micro behaviours of actors and observe macro trends in the system.

> TODO: Next we will introduce a second kind of actor with a different attitude and play with interactions.

## Design Goals

- Simplicity
  I hope it can be used by hobbysts (for fun) or in education (programming, ecology, systems thinking).

- Actor centric development 
  Applications should focus on programming actors. The macro behaviour of the system should be an amerging property of micro behaviours of actors.

- Aesthetics
  Smooth animations, nice colors, sharp shapes.

