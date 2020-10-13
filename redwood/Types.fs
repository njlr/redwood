namespace Redwood

open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics

[<Struct>]
type InputState =
  {
    Keyboard : KeyboardState
    Mouse : MouseState
  }

[<Struct>]
type Sprite =
  {
    Asset : string
    Position : Vector2
    Origin : Vector2
    SourceRectangle : Rectangle option
    Scale : Vector2
    Rotation : float32
    Color : Color
    Effects : SpriteEffects
    LayerDepth : float32
  }
  with
    static member Zero
      with get () =
        {
          Asset = ""
          Position = Vector2.Zero
          Origin = Vector2.Zero
          SourceRectangle = None
          Scale = Vector2.One
          Rotation = 0.0f
          Color = Color.White
          Effects = SpriteEffects.None
          LayerDepth = 0.0f
        }

[<Struct>]
type TextAlignment =
  | Left
  | Center
  | Right

[<Struct>]
type Text =
  {
    SpriteFontAsset : string
    Position : Vector2
    Text : string
    Color : Color
    Alignment : TextAlignment
  }
  with
    static member Zero
      with get () =
        {
          Text = ""
          SpriteFontAsset = ""
          Position = Vector2.Zero
          Color = Color.White
          Alignment = TextAlignment.Left
        }

[<Struct>]
type Renderable =
  | Sprite of sprite : Sprite
  | Text of text : Text

type CartridgeAction =
  | PlaySound of string
  | Exit

type Cartridge<'tstate> =
  {
    Initialize : Unit -> 'tstate
    Update : InputState -> 'tstate -> ('tstate * CartridgeAction list)
    Render : 'tstate -> Renderable list
  }
