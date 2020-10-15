module Redwood.Logic

open Aether

type Logic<'s1, 's2, 'ret, 'eff> = 's1 -> (('s2 * 'eff list) * 'ret)

// Bind : M<'T> * ('T -> M<'U>) -> M<'U>
let private bind (m : Logic<'s1, 's2, 't, 'eff>) (f : 't -> Logic<'s2, 's2, 'u, 'eff>) : Logic<'s1, 's2, 'u, 'eff> =
  (fun state ->
    let (state, actions), ret = m state
    let (state, nextActions), ret = (f ret) state
    (state, (actions @ nextActions)), ret)

// Return : 'T -> M<'T>
let private ret (x : 't) : Logic<'s, 's, 't, 'eff> =
  (fun s -> (s, []), x)

// ReturnFrom : M<'T> -> M<'T>
let private retFrom (m : Logic<'s, 's, 't, 'eff>) = m

// Zero : unit -> M<'T>
let private zero () : Logic<_, _, _, _> =
  (fun s -> (s, []), ())

// Combine : M<unit> * M<'T> -> M<'T>
let private combine (x : Logic<'s, 's, 't, 'eff>) (y : Logic<'s, 's, 't, 'eff>) : Logic<'s, 's, 't, 'eff> =
  (fun state ->
    let (state, actions), _ = x state
    let (state, nextActions), ret = y state
    (state, (actions @ nextActions)), ret)

// Delay : (unit -> M<'T>) -> M<'T>
let private delay f : Logic<_, _, _, _> = f ()

// For : seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
// For : seq<'T> * ('T -> M<'U>) -> M<'U>
let private forDo (xs : seq<'t>) (f : 't -> Logic<'s, 's, Unit, _>) : Logic<'s, 's, Unit, _> =
  (fun state ->
    xs
    |> Seq.fold
      (fun acc x ->
        let (state, actions), () = acc
        let (nextState, nextActions), () = (f x) state
        (nextState, actions @ nextActions), ())
      ((state, []), ()))

type LogicBuilder () =
  member this.Bind (m, f) = bind m f
  member this.Return x = ret x
  member this.ReturnFrom x = retFrom x
  member this.Combine (x, y) = combine x y
  member this.Zero () = zero ()
  member this.Delay f = delay f
  member this.For (xs, f) = forDo xs f

[<AutoOpen>]
module ComputationExpression =

  let logic = LogicBuilder ()

[<RequireQualifiedAccess>]
module Logic =

  let getState : Logic<_, _, _, _> = (fun s -> (s, []), s)

  let setState nextState : Logic<_, _, _, _> = (fun _ -> (nextState, []), ())

  let mapState f : Logic<_, _, _, _> = (fun s -> (f s, []), ())

  let ignore (x : Logic<_, _, _, _>) : Logic<_, _, Unit, _> =
    (fun state ->
      let (state, actions), _ = x state
      (state, actions), ())

  let emitAll actions : Logic<_, _, Unit, _> =
    (fun state -> (state, actions), ())

  let emit action = emitAll [ action ]

  let zoom (lens : Lens<'outer, 'inner>) (x : Logic<'inner, 'inner, 'ret, _>) : Logic<'outer, 'outer, 'ret, _> =
    (fun outerState ->
      let innerState = Optic.get lens outerState
      let (innerState, actions), ret = x innerState
      let outerState = outerState |> Optic.set lens innerState
      (outerState, actions), ret)

  let zoomPrism (lens : Prism<'outer, 'inner>) (x : Logic<'inner, 'inner, Unit, _>) : Logic<'outer, 'outer, Unit, _> =
    (fun outerState ->
      match Optic.get lens outerState with
      | Some innerState ->
        let (innerState, actions), () = x innerState
        let outerState = outerState |> Optic.set lens innerState
        (outerState, actions), ()
      | None ->
        (outerState, []), ())

  let transformActions handler (m : Logic<_, _, _, 'eff>) : Logic<_, _, _, 'eff2> =
    logic {
      let! state = getState

      let (state, actions), ret = m state

      do! setState state

      for action in actions do
        do! handler action

      return ret
    }

  let run (state : 'tstate) (logic : Logic<'tstate, 'tstate, Unit, _>) =
    let (state, actions), () = logic state
    state, actions

[<RequireQualifiedAccess>]
module Cartridge =

  let exit () = Logic.emit (Exit)

  let playSound (asset : string) = Logic.emit (PlaySound asset)

