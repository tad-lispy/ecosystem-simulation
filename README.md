Elm Actor Based Simulation Kit 

ReadME
A framework for creating simulations of systems consisting of autonomous actors interacting with each other. (e.g. an ecosystem)

Installation

It's an Elm package. Install it the usual way:

> elm install tad-lispy/elm-actors-simulation

Design Goals

- Simplicity
  I hope it can be used by hobbysts (for fun) or in education (programming, ecology, systems thinking).

- Actor centric development 
  Applications should focus on programming actors. The macro behaviour of the system should be an amerging property of micro behaviours of actors.

- Aesthetics
  Smooth animations, nice colors, sharp shapes.


Tutorial

I think it's easiest to explain by showing an example. Let's build a simple simulations.
Start by installing Elm and setting up a project:

> elm init

Then install this package:

> elm install tad-lispy/ecosystem

Create src/Main.elm as follows:

```elm
  module Main exposing (main)
  import Ecosystem
  import Color exposing (Color)

  Main =
        Ecosystem.simulation
                { size = Length.meters 500
                , update = update
                , paint = paint
                , init = init
                }

init =
      []
      Update Actor Environment duration =
          { movement = Vector2d.zero
          , state = Unemployed
          , spawn = []
          , interactions = []
          }
paint Actor
  actor =
    { size = Centimeters 20
    , fill = Colors.lightBlue
    , stroke = Colors.blue
    }
```

This should be enough to compile our program. Later we will discuss the code above, but first let's try to run it.

> elm reactor

In your browser open http://localhost:8000/src/Main.elm you should see a perfectly empty screen. That's ok since we don't have any actors in our system yet.

So,  what is the meaning of all of this?

First our main value. It's a standard Elm platform program. You can read more about it in the Official Elm Guide. We construct it by calling the Ecosystem.simulation function and passing it Ecosystem.Setup actor action record. We will get to actor and action type parameters later.

The setup record consists of four fields:

# size : Length

The simulation takes place on a square wrapped plane. This field defines the length of the edge of this square. Basically how big is the simulated world.

# init : List (Update actor action)

The initial setup of the simulation. We will discuss update type in a moment. The number of elements in this list will translate in to number of actors. Since the list is empty, there are no actors.

# update : Ecosystem.id ->
           Ecosystem.Groups ->
           Duration ->
           List(Interaction action) -> Update actor action

This function will be called for each actor on every frame of the animation. It will be passed the following arguments:

1. The Id of this actor

2. List of groups of other actors.

  Group is a record :

    { position : Vector2d
    , members : List Id
    }

Position is relative to the actor. Other actors are clustered in to groups for efficiency. The further away is the cluster, the bigger area it covers.

3. A list of interactions coming from other actors.

You can probably guess how it works, but the details are best explained with a concrete example so we will come back to thus later.

4. The time duration since previous update.

The update Actor function needs to return an update actor action record. We will discuss it in a moment (I promise!).

# Paint Actor : actor -> Image

How should your actor be represented on the screen. Image is a record.

```elm
  { size : Length -- the radius of the dot
  , fill : Color -- inside
  , stroke : Color -- outside (20%)
  }
```

Ok, so what id this whole Update actor action type? It's a record :

```elm
  { movement : Vector2d
  , change : Change actor
  , interactions : List (Interaction actor)
  , spawns : List (Update actor action)
  }
```

* The `movement` field controls in which direction and how far did the actor move.

* The change controls the internal state of the actor. It's a union type with the following constructors:

- Unchanged  the actor remains as it was

- Changed actor  the actor changes. Its new state is the tagged value of the actor.

- Removed the actor is to be removed from the simulation.

* The interactions field describes all actions taken by this actor towards other actors.
We will cover interactions later, for now we will just assign an empty list here, i.e there will be no interactions.

* The spawns field allows the actor to create new actors. It's a lot of `Update` actor action values. It works by creating a copy of the original actor and then applying the update, so if you set the change to Unchanged, you will get an exact copy of the parent. If you set it to `Removed` then the `Child` will be immediately removed. But then why spawn it in the first place?

| Tip : Make sure not to spawn multiple actors in the same place. Also, it's possible to recursively spawn, but let's not get crazy, alright?

We have setup some concepts and should be ready to add some actors to our simulation. First we will need to provide concrete types to  actor and action type parameters that we saw everywhere. Let's define them. The simplest type in Elm is unit ().

```elm

type alias Actor =
  ()

type alias Action =
  ()

```

Now we can plug these types in to our program:

```elm
init =
  [ Update
    { movement = Vector.zero
    , change = Changed ()
    , interactions = []
    , spawns = []
    }
  ]
```

That should be enough to produce a single actor - although without much agency yet. Reload the browser and observe a single motionless blue dot in the middle of the screen.

Let's add another one five meters to the north.

```elm
init =
  [ Update ...
  , Update
      { movement = Vector2d.meters 5 0
      , ...
      }
  ]
```
And now we have two perfectly still colors. Let's give them some agency. For thus we need to change the update function. For starters let's make the actors go east with a speed of 0,5 m/s :

```elm
update Actor Id groups interactions duration =
    { movement =
        speed.metersPerSecond 0,5
            |> Quantity.for duration
            |> Vector2d.meters 0
      ...
    }
````

Reload the browser and your actors should march across the screen at a steady pace of 50cm per second. Notice that once they reach the "edge" they will re-appear on the other side. That's what we mean by wrapped plane. It works the same for left-right and up-down movement. In fact for the actors there is no such thing as the edge of the earth for us (take that, flat earthers).

I hope you didn't find it too difficult so far, but also let's agree that it's not very interesting yet. Our actors are pretty dumb and stubborn. How about giving them the following behaviours :

- They will move away from each other, as if disgusted by one another.

- If there is no other actor in 50 m radius, they will spawn a single, equally disgusting child.

Sounds like fun. Let's analyse the problem. First the movement.

Actor needs to know where is the nearest other actor. Fortunately this information is provided to the update Actor function as a second argument. It contains a list of all other actors, grouped in to clusters. Each group comes with a position relative to the actor and list of its members. We are only interested in the nearest group. We can get it by sorting the list by distance and taking the first element if any (the head) :

```elm
    groups
      |> List.sortBy (.position >> Vector2d.length)
      |> List.head
```

If we have the result as nearest then we can define movement as :

```elm
    case nearest of
      Nothing ->
          I am the only actor here!
          Vector.zero
      Just group ->  Vector2d.normalize
let direction = group.position
                    |> Vector2d.reverse
                    |> Vector2d.direction
in
    direction |> Maybe.map (Vector2d.with length (meters 0,5))
    |> Maybe.withDefault (Vector2d.zero)
    |> Vector2d.for duration
```
What with all the Maybe s?

First it may be that there are no other actors in this simulation. Then List of groups would be empty and its head would be Nothing.

Second maybe concerns the direction. Truth be told it should never happen - if the other actor is exactly in the same spot as this one it would be omitted in groups. It theoretically can happen that another actor is at the same point as this one. In that case there would be no sense  to talk about a direction. So in that case we don't move either (Vector2d.zero).

That should solve the movement one.

Let's plug it in and see :

```elm
    { movement = Movement
    , ...
    }
```
In the browser we should observe that the two actors move away from each other until they are half the world apart. At this stage they start to shake. So strong is their mutual repulsion that they go to the opposite sides of their world and still try to go further away. Unfortunately even step brings them closer together, so immediately they take a step back. Hell is other actors.

Ok, now what about spawning. First we need to know the distance to the nearest group. But wait! What if there is no other group (nearest is Nothing)? Let's also consider where to place our beloved child. Of course away from those nasty other actors. Only if there is no other's then there is no "away" direction. We need to make some design decisions.

We could just decide that the actor should remain the only on in the simulation. Why spoil the perfect state of loneliness. But that would be rather boring for us to watch.

So why not spawn three nasty little actors and let them run away in all directions? Let's do that!

```elm
spawns =
    case nearest of
        Nothing ->
            [ { movement = Vector2d.with length
                  Directio2d.positive X (Meters 0,5)
              }
            ]
        Just group ->
            case Vector2d.direction group.position of
                Nothing ->
                    [] -- this should never happen, but we need to satisfy the type system.
                Just direction ->
                    [ { movement = ...
                      , ...
                      }
                    ]
```

This should do the trick! Lets see!

Now notice that if you wait long enough the actors should fill the space more or less evenly and stop reproducing.

Interestingly we didn't really program them to do so. We have just instilled a deeply rooted hatred to one another and strong urge to reproduce. And here they are concurring the world and exploiting every last bit of it. May be there is a lesson here?

This is what I would call an emerging property. We program micro behaviours of actors and observe macro trends in the system.

Next we will introduce a second type of actor with a different attitude and play with interactions.
