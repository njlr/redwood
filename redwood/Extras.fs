namespace Redwood.Extras

open Microsoft.Xna.Framework

module Vector2 =

  let create x y : Vector2 =
    Vector2 (x, y)

  let ofPoint (p : Point) =
    create (float32 p.X) (float32 p.Y)

  let lengthSquared (v : Vector2) =
    v.LengthSquared ()

  let length (v : Vector2) =
    v.Length ()

  let normalize (v : Vector2) =
    let ls = lengthSquared v

    if ls = 0.0f
    then
      v
    else
      v / (sqrt ls)

  let truncate (maxLength : float32) (v : Vector2) =
    let ls = v.LengthSquared ()

    if ls * ls > maxLength
    then
      (v * maxLength) / sqrt ls
    else
      v

  let toAngle (v : Vector2) =
    atan2 v.Y v.X

module Option =

  open Aether

  let valueOrDefault (fallback : 't) : Lens<'t option, 't> =
    (fun x -> x |> Option.defaultValue fallback), (fun v _ -> Some v)
