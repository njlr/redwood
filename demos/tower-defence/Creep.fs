module Redwood.Demos.TowerDefence.Logic.Creep

open Aether
open Aether.Operators
open Redwood
open Redwood.Logic
open Redwood.Extras
open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Redwood.Demos.TowerDefence

module Optics =

  let jumpTimer : Lens<_, _> =
    (fun x -> x.JumpTimer), (fun v x -> { x with JumpTimer = v })

let private findNearestTurret (world : World) (position : Vector2) =
  world.Entities
  |> Map.toSeq
  |> Seq.choose (fun (k, v) -> match v with | Tower t -> Some (k, t) | _ -> None)
  |> Seq.sortBy (fun (_, t) -> t.Position - position |> Vector2.lengthSquared)
  |> Seq.tryHead

let private trySelectNewTarget (world : World) : Logic<Creep, _, _, _> =
  logic {
    let! state = Logic.getState

    let x = findNearestTurret world state.Position

    do! Logic.setState { state with TargetEntity = x |> Option.map fst }

    return x |> Option.map snd
  }

let update (world : World) : Logic<Creep, _, _, _> =
  logic {
    let! state = Logic.getState

    let! targetTurret =
      logic {
        match state.TargetEntity with
        | Some id ->
          match world.Entities |> Map.tryFind id with
          | Some (Tower t) ->
            return Some t
          | _ ->
            return! trySelectNewTarget world
        | None ->
          return! trySelectNewTarget world
      }

    if state.JumpTimer > 0
    then
      do!
        Logic.mapState ((fun x -> x - 1) ^% Optics.jumpTimer)
    else
      match targetTurret with
      | Some turret ->
        let desiredVelocity =
          Steering.seek state.Position turret.Position

        let steering = Steering.steer state.Velocity desiredVelocity 4.0f

        do!
          Logic.setState
            {
              state with
                JumpTimer = 60
                Velocity = state.Velocity + steering
            }
      | None -> ()

    // Update position and velocity
    let! state = Logic.getState

    let nextVelocity = state.Velocity * 0.95f

    do!
      Logic.setState
        {
          state with
            Position = state.Position + nextVelocity
            Velocity = nextVelocity
        }
  }
