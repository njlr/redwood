module Redwood.Demos.Boids

open System
open Aether
open Aether.Operators
open Redwood
open Redwood.Logic
open Redwood.Extras
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

[<Struct>]
type Boid =
  {
    Position : Vector2
    Velocity : Vector2
  }

type State =
  {
    WasPressed : bool
    Boids : Map<Guid, Boid>
  }

module State =

  let boids : Lens<State, _> =
    (fun x -> x.Boids), (fun v x -> { x with Boids = v })

let spawnBoid (position : Vector2) =
  logic {
    let id = Guid.NewGuid ()

    let! state = Logic.getState

    do!
      Logic.setState
        {
          state with
            Boids =
              state.Boids
              |> Map.add
                id
                {
                  Position = position
                  Velocity = Vector2.Zero + Vector2.create 0.1f 0.1f
                }
        }

    return id
  }

let findNeighbours (position : Vector2) (radius : float32) (world : Map<Guid, Boid>) =
  world
  |> Map.toSeq
  |> Seq.filter (fun (k, v) ->
    Vector2.lengthSquared (v.Position - position) <= radius * radius
  )

let updateBoid (input : InputState) (self : Guid) (world : Map<Guid, Boid>) =
  logic {
    let! state = Logic.getState

    let separationRange = 128.0f

    let neighbours =
      findNeighbours state.Position separationRange world
      |> Seq.filter (fun (id, _) -> id <> self)
      |> Seq.toList

    let separation =
      if List.isEmpty neighbours
      then
        Vector2.Zero
      else
        let sum =
          neighbours
          |> Seq.sumBy
            (fun (_, other) ->
              let dv = other.Position - state.Position
              let amount = Vector2.length dv / separationRange
              Vector2.SmoothStep (dv, Vector2.Zero, amount)
            )

        sum / float32 (List.length neighbours)

    let seek =
      Steering.seek
        state.Position
        (Vector2.ofPoint input.Mouse.Position)

    let steering = seek * 0.3f + separation * 0.7f

    let nextVelocity =
      state.Velocity * 0.95f +
      (Steering.steer state.Velocity steering 0.4f)

    do!
      Logic.mapState
        (fun (state : Boid) ->
          {
            state with
              Velocity = nextVelocity
              Position = state.Position + nextVelocity
          })
  }

let initialize () =
  {
    WasPressed = false
    Boids = Map.empty
  }

let update (input : InputState) state =
  logic {
    let! state = Logic.getState

    let isPressed = input.Mouse.LeftButton = ButtonState.Pressed

    if isPressed && (not state.WasPressed)
    then
      do!
        spawnBoid (Vector2.ofPoint input.Mouse.Position)
        |> Logic.ignore

    let! state = Logic.getState

    for id, boid in state.Boids |> Map.toSeq do
      let optic =
        State.boids
        >-> Map.value_ id
        >-> Option.valueOrDefault boid

      do! Logic.zoom optic (updateBoid input id state.Boids)

    do! Logic.mapState (fun state -> { state with WasPressed = isPressed })
  }
  |> Logic.run state

let render state =
  seq {
    for _, boid in state.Boids |> Map.toSeq do
      yield
        {
          Sprite.Zero with
            Asset = "assets/boid.png"
            Origin = Vector2.create 256.0f 256.0f
            Scale = Vector2.create 0.1f 0.1f
            Position = boid.Position
            Rotation = Vector2.toAngle boid.Velocity
        }
  }
  |> Seq.toList

let cart : Cartridge<_> =
  {
    Initialize = initialize
    Update = update
    Render = render
  }
