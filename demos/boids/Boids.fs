module Redwood.Demos.Boids

open System
open Aether
open Aether.Operators
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Redwood
open Redwood.Logic
open Redwood.EntitySystem
open Redwood.Linear

[<Struct>]
type Boid =
  {
    Position : Vector2F32
    Velocity : Vector2F32
  }

type State =
  {
    PreviousInput : InputState
    Entities : EntitySystem<Boid>
  }

module Boid =

  let private findNeighbours (radius : float32) (context : EntityContext<_, _>) =
    logic {
      let! state = Logic.getState

      return
        context.World
        |> Map.toSeq
        |> Seq.filter (fun (k, v) ->
          k <> context.Self &&
          Vector2.lengthSquared (v.Position - state.Position) <= radius * radius
        )
        |> Seq.toList
    }

  let update (input : InputState) (context : EntityContext<_, _>) =
    logic {
      let! state = Logic.getState

      let separationRange = 128.0f

      let! neighbours = findNeighbours separationRange context

      let separation =
        if List.isEmpty neighbours
        then
          Vector2.zero ()
        else
          let sum =
            neighbours
            |> Seq.sumBy
              (fun (_, other) ->
                let dv = other.Position - state.Position
                let amount = Vector2.length dv / separationRange
                Vector2.smoothStep dv (Vector2.zero ()) amount
              )

          sum / float32 (List.length neighbours)

      let mousePosition = 
        Vector2.ofXnaPoint input.Mouse.Position 
        |> Vector2.float32

      let seek =
        if input.Mouse.RightButton = ButtonState.Pressed
        then
          Steering.flee state.Position mousePosition
        else
          Steering.seek state.Position mousePosition

      let steering = seek * 0.2f + separation * 0.8f

      let nextVelocity =
        state.Velocity * 0.98f +
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

module Optics =

  let previousInput : Lens<State, _> =
    (fun x -> x.PreviousInput), (fun v x -> { x with PreviousInput = v })

  let entities : Lens<State, _> =
    (fun x -> x.Entities), (fun v x -> { x with Entities = v })

let initialize () =
  {
    PreviousInput = InputState.Zero
    Entities = EntitySystem.empty
  }

let update (input : InputState) state =
  logic {
    // Press escape to exit
    if input.Keyboard.IsKeyDown Keys.Escape
    then
      do! Cartridge.exit ()

    let! state = Logic.getState

    let isPressed = input.Mouse.LeftButton = ButtonState.Pressed
    let wasPressed = state.PreviousInput.Mouse.LeftButton = ButtonState.Pressed

    // Nested logic for entity system
    do!
      logic {
        // Click to create new boid
        if isPressed && (not wasPressed)
        then
          do!
            EntitySystem.spawnEntity
              {
                Position = Vector2.ofXnaPoint input.Mouse.Position |> Vector2.float32
                Velocity = Vector2.create 8.0f 4.0f
              }
            |> Logic.ignore

        // Update boids
        do! EntitySystem.updateEntities (Boid.update input)
      }
      |> Logic.zoom Optics.entities

    // Record the previous input
    do! Logic.mapState (input ^= Optics.previousInput)
  }
  |> Logic.run state

let render state =
  seq {
    yield
      Text
        {
          Text.Zero with
            SpriteFontAsset = "assets/fonts/OpenSans-Regular.ttf"
            Position = Vector2.create 4.0f 4.0f
            Alignment = TextAlignment.Left
            Text = "Left click to create a boid; Right click to scare boids"
        }

    for _, boid in state.Entities |> Map.toSeq do
      yield
        Sprite
          {
            Sprite.Zero with
              Asset = "assets/sprites/boid.png"
              Origin = Vector2.create 256.0f 256.0f
              Scale = Vector2.create 0.1f 0.1f
              Position = boid.Position
              Rotation = Vector2.toAngle boid.Velocity
          }
  }
  |> Seq.toList

let cartridge : Cartridge<_> =
  {
    Initialize = initialize
    Update = update
    Render = render
  }
