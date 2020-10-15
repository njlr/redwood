namespace Redwood

open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics
open Redwood.Linear
open Redwood.Content

type RedwoodDriver<'tstate when 'tstate : equality> (cart : Cartridge<'tstate>) as this =
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
        match action with
        | Exit -> this.Exit ()
        | _ ->
          printfn "Action: %A" action

      state <- nextState

    override this.Draw (gameTime) =
      graphics.GraphicsDevice.Clear Color.CornflowerBlue

      spriteBatch.Begin ()

      let renderables = cart.Render state

      for renderable in renderables do
        match renderable with
        | Sprite sprite ->
          match contentManager.LoadTexture sprite.Asset with
          | Ok texture ->
            spriteBatch.Draw
              (
                texture,
                Vector2.toXnaVector2 sprite.Position,
                sprite.SourceRectangle |> Option.toNullable,
                sprite.Color,
                sprite.Rotation,
                Vector2.toXnaVector2 sprite.Origin,
                Vector2.toXnaVector2 sprite.Scale,
                sprite.Effects,
                sprite.LayerDepth
              )
          | _ -> ()
        | Text text ->
          match contentManager.LoadSpriteFont text.SpriteFontAsset with
          | Ok spriteFont ->
            spriteBatch.DrawString
              (
                spriteFont,
                text.Text,
                Vector2.toXnaVector2 text.Position,
                text.Color
              )
          | _ -> ()

      spriteBatch.End ()
