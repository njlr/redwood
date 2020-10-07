namespace Redwood.Extras

open Microsoft.Xna.Framework

module Vector2 =

  let create x y : Vector2 =
    Vector2 (x, y)

  let ofPoint (p : Point) =
    create (float32 p.X) (float32 p.Y)

  let normalize (v : Vector2) =
    let u = v
    u.Normalize ()
    u

  let truncate (maxLength : float32) (v : Vector2) =
    let ls = v.LengthSquared ()

    if ls * ls > maxLength
    then
      (v * maxLength) / sqrt ls
    else
      v

  let lengthSquared (v : Vector2) =
    v.LengthSquared ()

  let length (v : Vector2) =
    v.Length ()

  let toAngle (v : Vector2) =
    atan2 v.Y v.X

module Option =

  open Aether

  let valueOrDefault (fallback : 't) : Lens<'t option, 't> =
    (fun x -> x |> Option.defaultValue fallback), (fun v _ -> Some v)
