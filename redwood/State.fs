module Redwood.State

type private State<'tstate, 'tresult> = 'tstate -> ('tstate * 'tresult)

// Bind : M<'T> * ('T -> M<'U>) -> M<'U>
let private bind (m : State<'tstate, 'tin>) (f : 'tin -> State<'tstate, 'tout>) : State<'tstate, 'tout> =
  (fun state ->
    let state, result = m state
    let state, result = (f result) state
    state, result)

// Return : 'T -> M<'T>
let private ret (x : 'tresult) : State<'tstate, 'tresult> =
  (fun s -> s, x)

// Zero : unit -> M<'T>
let private zero () : State<_, Unit> =
  (fun s -> s, ())

// Combine : M<unit> * M<'T> -> M<'T>
let private combine (x : State<'tstate, Unit>) (y : State<'tstate, 'tresult>) : State<'tstate, 'tresult> =
  (fun state ->
    let state, () = x state
    y state)

// Delay : (unit -> M<'T>) -> M<'T>
let private delay (f) : State<_, _> = f ()

type StateBuilder () =
  member this.Bind (m, f) = bind m f
  member this.Return x = ret x
  member this.Combine (x, y) = combine x y
  member this.Zero () = zero ()
  member this.Delay f = delay f

[<RequireQualifiedAccess>]
module State =

  let get : State<_, _> = (fun s -> s, s)

  let set nextState : State<_, _> = (fun _ -> nextState, ())

  let map f : State<_, _> = (fun s -> f s, ())

  let ignore (x : State<'tstate, _>) : State<'tstate, Unit> =
    (fun state ->
      let state, _ = x state
      state, ())

  // let inline run state (x : State<_, _>) =
  //   x state

[<AutoOpen>]
module ComputationExpression =

  let state = StateBuilder ()
