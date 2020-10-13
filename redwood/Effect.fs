module Redwood.Effect

open Redwood

type private Effect<'tin, 'tout, 'teff> = 'tin -> 'tout * ('teff list)

// Bind : M<'T> * ('T -> M<'U>) -> M<'U>
let private bind (m : Effect<'t, 'u, 'teff>) (f : 'u -> Effect<'u, 's, 'teff>) : Effect<'t, 's, 'teff> =
  (fun state ->
    let state, actions1 = m state
    let state, actions2 = (f state) state
    state, actions1 @ actions2)

// Return : 'T -> M<'T>
let private ret (x : 'tout) : Effect<'tin, 'tout, 'teff> =
  (fun _ -> x, [])

// Zero : unit -> M<'T>
let private zero () : Effect<'t, 't, 'teff> =
  (fun s -> s, [])

// Combine : M<unit> * M<'T> -> M<'T>
let private combine (x : Effect<Unit, Unit, 'teff>) (y : Effect<'tin, 'tout, 'teff>) : Effect<'tin, 'tout, 'teff> =
  (fun state ->
    let (), actions1 = x ()
    let state, actions2 = y state
    state, actions1 @ actions2)

// Delay : (unit -> M<'T>) -> M<'T>
let private delay (f) : Effect<_, _, _> = f ()

type EffectBuilder () =
  member this.Bind (m, f) = bind m f
  member this.Return x = ret x
  member this.Combine (x, y) = combine x y
  member this.Zero () = zero ()
  member this.Delay f = delay f

[<RequireQualifiedAccess>]
module Effect =

  let emit action : Effect<Unit, Unit, _> = (fun () -> (), [ action ])

  let emitAll actions : Effect<Unit, Unit, _> = (fun () -> (), actions)

  let run x (e : Effect<_, _, _>) = e x

[<AutoOpen>]
module ComputationExpression =

  let effect = EffectBuilder ()
