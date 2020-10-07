namespace Redwood

open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics

type ContentManager (graphicsDevice) =
  let mutable textures = Map.empty

  member this.LoadTexture (path : string) =
    match textures |> Map.tryFind path with
    | Some x -> x
    | None ->
      try
        let t = Texture2D.FromFile (graphicsDevice, path)
        textures <- Map.add path (Ok t) textures
        Ok t
      with exn ->
        printfn "Error loading texture %s: %A" path exn
        textures <- Map.add path (Error exn) textures
        Error exn

  interface IDisposable with
    override this.Dispose () =
      for _, x in textures |> Map.toSeq do
        match x with
        | Ok texture -> texture.Dispose ()
        | _ -> ()

      textures <- Map.empty

type Driver<'tstate when 'tstate : equality> (cart : Cartridge<'tstate>) as this =
  inherit Game () with
    let graphics = new GraphicsDeviceManager (this)

    let mutable contentManager = Unchecked.defaultof<ContentManager>
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>

    let mutable state = Unchecked.defaultof<'tstate>

    member this.Graphics with get () = graphics

    override this.Initialize () =
      state <- cart.Initialize ()
      base.Initialize ()

    override this.LoadContent () =
      contentManager <- new ContentManager (graphics.GraphicsDevice)
      spriteBatch <- new SpriteBatch (graphics.GraphicsDevice)

    override this.UnloadContent () =
      (contentManager :> IDisposable).Dispose ()
      spriteBatch.Dispose ()

    override this.Update (gameTime) =
      let input =
        {
          Keyboard = Keyboard.GetState ()
          Mouse = Mouse.GetState ()
        }

      let nextState, actions = cart.Update input state

      for action in actions do
        printfn "Action: %A" action

      state <- nextState

    override this.Draw (gameTime) =
      graphics.GraphicsDevice.Clear Color.CornflowerBlue

      spriteBatch.Begin ()

      let sprites = cart.Render state

      for sprite in sprites do
        match contentManager.LoadTexture sprite.Asset with
        | Ok texture ->
          spriteBatch.Draw
            (
              texture,
              sprite.Position,
              sprite.SourceRectangle |> Option.toNullable,
              sprite.Color,
              sprite.Rotation,
              sprite.Origin,
              sprite.Scale,
              sprite.Effects,
              sprite.LayerDepth
            )
        | _ -> ()

      spriteBatch.End ()
