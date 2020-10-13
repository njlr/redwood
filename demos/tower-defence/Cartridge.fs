module Redwood.Demos.TowerDefence.Cartridge

open Aether
open Aether.Operators
open Redwood
open Redwood.Logic
open Redwood.Extras
open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Redwood.Demos.TowerDefence.Logic

module Entity =

  let tower : Prism<Entity, Tower> =
    (function | Tower t -> Some t | _ -> None), (fun v _ -> Tower v)

  let creep : Prism<Entity, Creep> =
    (function | Creep c -> Some c | _ -> None), (fun v _ -> Creep v)

module World =

  let private findNearestCreep (world : World) (position : Vector2) =
    world.Entities
    |> Map.toSeq
    |> Seq.choose (fun (k, v) -> match v with | Creep t -> Some (k, t) | _ -> None)
    |> Seq.sortBy (fun (_, t) -> t.Position - position |> Vector2.lengthSquared)
    |> Seq.tryHead

  let entities : Lens<World, _> =
    (fun x -> x.Entities), (fun v x -> { x with Entities = v })

  let private trySpawnTurret (position : Vector2) =
    logic {
      let! state = Logic.getState

      if state.Credits > 100
      then
        let id = Guid.NewGuid ()

        do!
          Logic.setState
            {
              state with
                Credits = state.Credits - 100
                Entities =
                  state.Entities
                  |> Map.add
                    id
                    (
                      Tower
                        {
                          Position = position
                          WeaponCooldown = 0
                        }
                    )
            }

        return Some id
      else
        return None
    }

  let private fireBullet (position : Vector2) (direction : Vector2) =
    logic {
      let! state = Logic.getState

      let bullet : Bullet =
        {
          Life = 180
          Position = position
          Velocity =
            direction
            |> Vector2.normalize |> (*) 1.4f
        }

      do!
        Logic.setState
          {
            state with
              Bullets =
                bullet :: state.Bullets
          }
    }

  let private handleAction action =
    logic {
      match action with
      | CreateBullet (position, direction) ->
        do! fireBullet position direction
    }

  let update (input : InputState) =
    logic {
      if input.Keyboard.IsKeyDown Keys.Escape
      then
        do! Logic.emit Exit

      // Place turrets
      let! state = Logic.getState

      if
        input.Mouse.LeftButton = ButtonState.Released &&
        state.PreviousInput.Mouse.LeftButton = ButtonState.Pressed
      then
        do!
          trySpawnTurret (Vector2.ofPoint input.Mouse.Position)
          |> Logic.ignore

      // Spawn creeps
      let! state = Logic.getState

      if state.CreepSpawnTimer > 0
      then
        do! Logic.setState { state with CreepSpawnTimer = state.CreepSpawnTimer - 1 }
      else
        let creepId = Guid.NewGuid ()

        let creep =
          Creep
            {
              Position = Vector2.create 32.0f 32.0f
              Velocity = Vector2.Zero
              HitPoints = 10
              MaxHitPoints = 10
              TargetEntity = None
              JumpTimer = 0
            }

        do!
          Logic.setState
            {
              state with
                CreepSpawnTimer = 60
                Entities =
                  state.Entities
                  |> Map.add creepId creep
            }

      // Update entities
      let! state = Logic.getState

      for (id, entity) in state.Entities |> Map.toSeq do
        match entity with
        | Tower _ ->
          let optic =
            entities
            >-> Map.value_ id
            >-> Option.value_
            >?> Entity.tower

          do!
            Logic.zoomPrism optic (Tower.update state)
            |> Logic.transformActions handleAction
        | Creep _ ->
          let optic =
            entities
            >-> Map.value_ id
            >-> Option.value_
            >?> Entity.creep

          do!
            Logic.zoomPrism optic (Creep.update state)
            |> Logic.transformActions handleAction

      // Update bullets
      do!
        Logic.mapState
          (fun state ->
            {
              state with
                Bullets =
                  state.Bullets
                  |> List.map
                    (fun b ->
                      {
                        b with
                          Life = b.Life - 1
                          Position = b.Position + b.Velocity
                      })
                  |> List.filter (fun b -> b.Life > 0)
            })

      // Earn money
      let! state = Logic.getState

      do! Logic.setState { state with Credits = state.Credits + 1 }

      // Update the previous input
      do! Logic.mapState (fun s -> { s with PreviousInput = input })
    }

let initialize () =
  {
    Credits = 10
    Entities = Map.empty
    Bullets = []
    PreviousInput =
      {
        Keyboard = Unchecked.defaultof<_>
        Mouse = Unchecked.defaultof<_>
      }
    CreepSpawnTimer = 0
  }

let update input (state : World) =
  World.update input
  |> Logic.run state

let render (state) =
  seq {
    yield
      Text
        {
          Text.Zero with
            SpriteFontAsset = "assets/fonts/OpenSans-Regular.ttf"
            Text = string state.Credits
        }

    for bullet in state.Bullets do
      yield
        Sprite
          {
            Sprite.Zero with
              Asset = "assets/sprites/bullet.png"
              Origin = Vector2.create 8.0f 8.0f
              Position = bullet.Position
          }

    for _, entity in state.Entities |> Map.toSeq do
      match entity with
      | Tower tower ->
        yield
          Sprite
            {
              Sprite.Zero with
                Asset = "assets/sprites/turret.png"
                Origin = Vector2.create 8.0f 8.0f
                Position = tower.Position
            }
      | Creep creep ->
        yield
          Sprite
            {
              Sprite.Zero with
                Asset = "assets/sprites/creep.png"
                Origin = Vector2.create 8.0f 8.0f
                Color = Color.Red
                Position = creep.Position
            }
  }
  |> Seq.toList

let cartridge : Cartridge<_> =
  {
    Initialize = initialize
    Update = update
    Render = render
  }
