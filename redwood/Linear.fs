namespace Redwood.Linear

// The MonoGame Vector and Point types do not work well in
// F# data-structures since they lack the comparison attribute.
// Therefore, we need to re-implement these as F# records.

[<Struct>]
type Vector2<'t  when 't : equality
                  and 't : comparison
                  and 't : (static member get_Zero : Unit -> 't)
                  and 't : (static member (+) : 't * 't -> 't)
                  and 't : (static member (-) : 't * 't -> 't)
                  and 't : (static member (*) : 't * 't -> 't)
                  and 't : (static member (/) : 't * 't -> 't)> =
  {
    X : 't
    Y : 't
  }
  with
    // This is a hack to enable LanguagePrimitives.GenericZero
    // fsharplint:disable-next-line MemberNames
    static member inline get_Zero () : Vector2<'a> =
      {
        X = LanguagePrimitives.GenericZero
        Y = LanguagePrimitives.GenericZero
      }

    static member inline (+) (u : Vector2<'a>, v : Vector2<'a>) : Vector2<'a> =
      {
        X = u.X + v.X
        Y = u.Y + v.Y
      }

    static member inline (-) (u : Vector2<'a>, v : Vector2<'a>) : Vector2<'a> =
      {
        X = u.X - v.X
        Y = u.Y - v.Y
      }

    static member inline (*) (v : Vector2<'a>, k : 'a) : Vector2<'a> =
      {
        X = v.X * k
        Y = v.Y * k
      }

    static member inline (/) (v : Vector2<'a>, k : 'a) : Vector2<'a> =
      {
        X = v.X / k
        Y = v.Y / k
      }

    static member inline (/) (u : Vector2<'a>, v : Vector2<'a>) : Vector2<'a> =
      {
        X = u.X / v.X
        Y = u.Y / v.Y
      }

type Vector2I = Vector2<int>

type Vector2F32 = Vector2<float32>

module Vector2 =

  let inline zero () : Vector2<'a> = LanguagePrimitives.GenericZero

  let inline create x y : Vector2<_> =
    {
      X = x
      Y = y
    }

  let inline int v : Vector2I =
    create (int v.X) (int v.Y)

  let inline float32 v : Vector2F32 =
    create (float32 v.X) (float32 v.Y)

  let inline lengthSquared (v : Vector2<_>) =
    v.X * v.X + v.Y * v.Y

  let inline length (v : Vector2<'a>) : 'a =
    sqrt (lengthSquared v)

  let inline normalize (v : Vector2<_>) =
    let ls = lengthSquared v

    if ls = LanguagePrimitives.GenericZero
    then
      v
    else
      v / (sqrt ls)

  let inline truncate maxLength (v : Vector2<_>) =
    let ls = lengthSquared v

    if ls * ls > maxLength
    then
      v * (maxLength / sqrt ls)
    else
      v

  let inline toAngle (v : Vector2<'a>) : 'a =
    atan2 v.Y v.X

  let inline smoothStep (u : Vector2<'a>) (v : Vector2<'a>) (t : 'a) : Vector2<'a> =
    let two = LanguagePrimitives.GenericOne + LanguagePrimitives.GenericOne
    let three = two + LanguagePrimitives.GenericOne

    let inline clamp x lowerLimit upperLimit =
      if x < lowerLimit
      then
        lowerLimit
      else if x > upperLimit
      then
        upperLimit
      else
        x

    let inline smoothStep edge0 edge1 t =
      let t =
        clamp
          ((t - edge0) / (edge1 - edge0))
          LanguagePrimitives.GenericZero
          LanguagePrimitives.GenericOne
      t * t * (three - two * t)

    create (smoothStep u.X v.X t) (smoothStep u.Y v.Y t)

  let ofXnaVector2 (v : Microsoft.Xna.Framework.Vector2) : Vector2<_> =
    create v.X v.Y

  let ofXnaPoint (p : Microsoft.Xna.Framework.Point) : Vector2<_> =
    create p.X p.Y

  let inline toXnaVector2 (v : Vector2<_>) : Microsoft.Xna.Framework.Vector2 =
    Microsoft.Xna.Framework.Vector2 (v.X, v.Y)

  let inline toXnaPoint (v : Vector2<_>) : Microsoft.Xna.Framework.Point =
    Microsoft.Xna.Framework.Point (v.X, v.Y)
