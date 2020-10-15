namespace Redwood.Linear

// TODO: Investigate if this is more efficient than the generic version.

// The MonoGame Vector and Point types do not work well in
// F# data-structures since they lack the comparison attribute.
// Therefore, we need to re-implement these as F# records.

[<Struct>]
type Vector2 =
  {
    X : float32
    Y : float32
  }
  with
    static member Zero
      with get () =
        {
          X = 0.0f
          Y = 0.0f
        }

    static member inline (+) (u, v) =
      {
        X = u.X + v.X
        Y = u.Y + v.Y
      }

    static member inline (-) (u, v) =
      {
        X = u.X + v.X
        Y = u.Y + v.Y
      }

    static member inline (*) (v, k) =
      {
        X = v.X * k
        Y = v.Y * k
      }

    static member inline (/) (v, k) =
      {
        X = v.X / k
        Y = v.Y / k
      }

[<Struct>]
type Point =
  {
    X : int
    Y : int
  }
  with
    static member Zero
      with get () =
        {
          X = 0
          Y = 0
        }

    static member inline (+) (u, v) =
      {
        X = u.X + v.X
        Y = u.Y + v.Y
      }

    static member inline (-) (u, v) =
      {
        X = u.X + v.X
        Y = u.Y + v.Y
      }

    static member inline (*) (v, k) =
      {
        X = v.X * k
        Y = v.Y * k
      }

    static member inline (/) (v, k) =
      {
        X = v.X / k
        Y = v.Y / k
      }

module Vector2 =

  let create x y : Vector2 =
    {
      X = x
      Y = y
    }

  let ofPoint (p : Point) : Vector2 =
    create (float32 p.X) (float32 p.Y)

  let ofXnaPoint (p : Microsoft.Xna.Framework.Point) : Vector2 =
    create (float32 p.X) (float32 p.Y)

  let inline lengthSquared (v : Vector2) =
    v.X * v.X + v.Y * v.Y

  let inline length (v : Vector2) =
    v |> lengthSquared |> sqrt

  let inline normalize (v : Vector2) =
    let ls = lengthSquared v

    if ls = LanguagePrimitives.GenericZero
    then
      v
    else
      v / (sqrt ls)

  let inline truncate maxLength (v : Vector2) =
    let ls = lengthSquared v

    if ls * ls > maxLength
    then
      (v * maxLength) / sqrt ls
    else
      v

  let inline toAngle (v : Vector2) =
    atan2 v.Y v.X

module Point =

  let create x y : Point =
    {
      X = x
      Y = y
    }

  let ofXnaPoint (p : Microsoft.Xna.Framework.Point) : Point =
    create p.X p.Y
