namespace Redwood.Content

open System
open System.IO
open Microsoft.Xna.Framework.Graphics
open SpriteFontPlus

type ContentManager (graphicsDevice) =
  let mutable textures = Map.empty
  let mutable spriteFonts = Map.empty

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

  member this.LoadSpriteFont (path : string) =
    match spriteFonts |> Map.tryFind path with
    | Some x -> x
    | None ->
      try
        let bytes = File.ReadAllBytes path

        let fontBakeResult =
          TtfFontBaker.Bake
            (
              bytes,
              24.0f,
              1024,
              1024,
              [|
                CharacterRange.BasicLatin
                CharacterRange.Latin1Supplement
                CharacterRange.LatinExtendedA
                CharacterRange.Cyrillic
              |]
            )

        let f = fontBakeResult.CreateSpriteFont(graphicsDevice)
        spriteFonts <- Map.add path (Ok f) spriteFonts
        Ok f
      with exn ->
        printfn "Error loading font %s: %A" path exn
        spriteFonts <- Map.add path (Error exn) spriteFonts
        Error exn

  interface IDisposable with
    override this.Dispose () =
      let disposeAll m =
        for _, x in m |> Map.toSeq do
          match x with
          | Ok r -> (r :> IDisposable).Dispose ()
          | _ -> ()

      disposeAll textures
      textures <- Map.empty

      spriteFonts <- Map.empty
