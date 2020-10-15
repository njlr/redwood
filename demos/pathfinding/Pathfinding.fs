module Redwood.Demos.Pathfinding

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Redwood
open Redwood.Linear
open Redwood.Logic
open Redwood.Pathfinding

type State =
  {
    Start : Vector2I
    Target : Vector2I
    Obstacles : Set<Vector2I>
    Path : Path<Vector2I> option
  }

let private graphOfState (state : State) =
  {
    new IGraph<Vector2I, int> with
      member this.GetNeighbours node =
        seq {
          yield node + Vector2.create -1 0
          yield node + Vector2.create 1 0
          yield node + Vector2.create 0 1
          yield node + Vector2.create 0 -1
        }
        |> Seq.filter
          (fun p ->
            p.X >= 0
            && p.Y >= 0
            && p.X < 50
            && p.Y < 30
            && not (Seq.contains p state.Obstacles))
        |> Seq.map (fun p -> p, 1)
  }

let private manhattan (p : Vector2I) (q : Vector2I) =
  abs (p.X - q.X) + abs (p.Y - q.Y)

let initialize () =
  {
    Start = Vector2.create 0 0
    Target = Vector2.create 0 0
    Obstacles =
      [
        Vector2.create 0 0
        Vector2.create 2 3
        Vector2.create 6 3
        Vector2.create 0 5
        Vector2.create 1 5
        Vector2.create 6 5
        Vector2.create 6 6
        Vector2.create 7 6
      ]
      |> Set.ofSeq
    Path = None
  }

let update input state =
  logic {
    if input.Keyboard.IsKeyDown Keys.Escape
    then
      do! Cartridge.exit ()

    let! initialState = Logic.getState

    let tileAtMouse = (Vector2.ofXnaPoint input.Mouse.Position) / 32

    if input.Mouse.LeftButton = ButtonState.Pressed
    then
      do!
        Logic.mapState
          (fun state ->
            { state with Start = tileAtMouse })

    if input.Mouse.RightButton = ButtonState.Pressed
    then
      do!
        Logic.mapState
          (fun state ->
            { state with Obstacles = Set.add tileAtMouse state.Obstacles })

    do!
      Logic.mapState
        (fun state ->
          { state with Target = tileAtMouse })

    let! state = Logic.getState

    if state <> initialState
    then
      let g = graphOfState state
      let path =
        Pathfinding.tryAStar
          manhattan
          state.Start
          state.Target
          g

      do!
        Logic.mapState
          (fun state ->
            { state with Path = path })
  }
  |> Logic.run state

let render state =
  seq {
    yield
      Sprite
        {
          Sprite.Zero with
            Asset = "assets/sprites/wall.png"
            Position = (Vector2.float32 state.Start) * 32.0f
            Color = Color.Green
        }

    yield
      Sprite
        {
          Sprite.Zero with
            Asset = "assets/sprites/wall.png"
            Position = (Vector2.float32 state.Target) * 32.0f
            Color = Color.Gold
        }

    for x in state.Obstacles do
      yield
        Sprite
          {
            Sprite.Zero with
              Asset = "assets/sprites/wall.png"
              Position = (Vector2.float32 x) * 32.0f
          }

    match state.Path with
    | Some path ->
      let l = List.length path

      for i, x in path |> Seq.indexed do
        yield
          Sprite
            {
              Sprite.Zero with
                Asset = "assets/sprites/wall.png"
                Position = (Vector2.float32 x) * 32.0f
                Color = Color.Lerp (Color.Gold, Color.Teal, float32 i / float32 l)
            }
    | None -> ()
  }
  |> Seq.toList

let cartridge =
  {
    Initialize = initialize
    Update = update
    Render = render
  }
