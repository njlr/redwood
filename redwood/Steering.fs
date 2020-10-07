module Redwood.Steering

open Microsoft.Xna.Framework
open Redwood.Extras

let seek (position : Vector2) (target : Vector2) =
  position - target

let flee (position : Vector2) (target : Vector2) =
  target - position

let steer (velocity : Vector2) (desiredVelocity : Vector2) (maxForce : float32) =
  velocity - desiredVelocity
  |> Vector2.truncate maxForce

