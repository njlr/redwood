module Redwood.Logic

open Aether

type private Logic<'s, 'ret> = 's -> (('s * CartridgeAction list) * 'ret)

// Bind : M<'T> * ('T -> M<'U>) -> M<'U>
let private bind (m : Logic<'s, 't>) (f : 't -> Logic<'s, 'u>) : Logic<'s, 'u> =
  (fun state ->
    let (state, actions), ret = m state
    let (state, nextActions), ret = (f ret) state
    (state, (actions @ nextActions)), ret)

// Return : 'T -> M<'T>
let private ret (x : 't) : Logic<'s, 't> =
  (fun s -> (s, []), x)

// Zero : unit -> M<'T>
let private zero () : Logic<_, _> =
  (fun s -> (s, []), ())

// Combine : M<unit> * M<'T> -> M<'T>
let private combine (x : Logic<'s, 't>) (y : Logic<'s, 't>) : Logic<'s, 't> =
  (fun state ->
    let (state, actions), _ = x state
    let (state, nextActions), ret = y state
    (state, (actions @ nextActions)), ret)

// Delay : (unit -> M<'T>) -> M<'T>
let private delay f : Logic<_, _> = f ()

// For : seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
// For : seq<'T> * ('T -> M<'U>) -> M<'U>
let private forDo (xs : seq<'t>) (f : 't -> Logic<'s, Unit>) : Logic<'s, Unit> =
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
  member this.Combine (x, y) = combine x y
  member this.Zero () = zero ()
  member this.Delay f = delay f
  member this.For (xs, f) = forDo xs f

[<RequireQualifiedAccess>]
module Logic =

  let getState : Logic<_, _> = (fun s -> (s, []), s)

  let setState nextState : Logic<_, _> = (fun _ -> (nextState, []), ())

  let mapState f : Logic<_, _> = (fun s -> (f s, []), ())

  let ignore (x : Logic<_, _>) : Logic<_, Unit> =
    (fun state ->
      let (state, actions), _ = x state
      (state, actions), ())

  let playSound (asset : string) : Logic<_, Unit> =
    (fun state -> (state, [ PlaySound asset ]), ())

  let zoom (lens : Lens<'outer, 'inner>) (x : Logic<'inner, 'ret>) : Logic<'outer, 'ret> =
    (fun outerState ->
      let innerState = Optic.get lens outerState
      let (innerState, actions), ret = x innerState
      let outerState = outerState |> Optic.set lens innerState
      (outerState, actions), ret)

  let zoomPrism (lens : Prism<'outer, 'inner>) (x : Logic<'inner, Unit>) : Logic<'outer, Unit> =
    (fun outerState ->
      match Optic.get lens outerState with
      | Some innerState ->
        let (innerState, actions), () = x innerState
        let outerState = outerState |> Optic.set lens innerState
        (outerState, actions), ()
      | None ->
        (outerState, []), ())

  let run (state : 'tstate) (logic : Logic<'tstate, Unit>) =
    let (state, actions), () = logic state
    state, actions

//   let map f : State<_, _> = (fun s -> f s, ())

//   let ignore (x : State<'tstate, _>) : State<'tstate, Unit> =
//     (fun state ->
//       let state, _ = x state
//       state, ())

  // let inline run state (x : State<_, _>) =
  //   x state

[<AutoOpen>]
module ComputationExpression =

  let logic = LogicBuilder ()
