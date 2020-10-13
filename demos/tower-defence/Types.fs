namespace Redwood.Demos.TowerDefence

open System
open Microsoft.Xna.Framework
open Redwood

type Tower =
  {
    Position : Vector2
    WeaponCooldown : int
  }

type Creep =
  {
    Position : Vector2
    Velocity : Vector2
    HitPoints : int
    MaxHitPoints : int
    TargetEntity : Guid option
    JumpTimer : int
  }

[<Struct>]
type Bullet =
  {
    Life : int
    Position : Vector2
    Velocity : Vector2
  }

type Entity =
  | Creep of Creep
  | Tower of Tower

[<Struct>]
type Action =
  | CreateBullet of Vector2 * Vector2

type World =
  {
    Credits : int
    Entities : Map<Guid, Entity>
    Bullets : Bullet list
    PreviousInput : InputState
    CreepSpawnTimer : int
  }
