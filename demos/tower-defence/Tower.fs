module Redwood.Demos.TowerDefence.Logic.Tower

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

  let weaponCooldown : Lens<_, _> =
    (fun x -> x.WeaponCooldown), (fun v x -> { x with WeaponCooldown = v })

let private tryFindNearestCreep (world : World) =
  logic {
    let! (state : Tower) = Logic.getState

    return
      world.Entities
      |> Map.toSeq
      |> Seq.choose
        (fun (k, v) ->
          match v with
          | Creep c -> Some (k, c)
          | _ -> None)
      |> Seq.sortBy (fun (k, v) -> v.Position - state.Position |> Vector2.lengthSquared)
      |> Seq.tryHead
  }

let update (world : World) : Logic<Tower, Tower, Unit, Action> =
  logic {
    let! state = Logic.getState

    if state.WeaponCooldown > 0
    then
      do! Logic.mapState ((fun x -> x - 1) ^% Optics.weaponCooldown)
    else
      match! tryFindNearestCreep world with
      | Some (_, creep) ->
        let direction = creep.Position - state.Position

        do! Logic.emit (CreateBullet (state.Position, direction))
        do! Logic.mapState (60 ^= Optics.weaponCooldown)

      | None -> ()
  }
