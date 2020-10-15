module Redwood.Steering

open Redwood.Linear

let inline seek (position : Vector2<_>) (target : Vector2<_>) =
  position - target

let inline flee (position : Vector2<_>) (target : Vector2<_>) =
  target - position

let inline steer (velocity : Vector2<_>) (desiredVelocity : Vector2<_>) (maxForce : _) =
  velocity - desiredVelocity
  |> Vector2.truncate maxForce
