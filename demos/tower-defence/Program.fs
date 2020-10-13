﻿open System
open Redwood
open Redwood.Demos.TowerDefence
open Redwood.Demos.TowerDefence.Cartridge

[<EntryPoint>]
let main argv =
  use game = new Driver<_> (cartridge)

  game.IsMouseVisible <- true

  let clientSizeHandler  =
    EventHandler<_>
      (fun _ _ ->
        game.Graphics.PreferredBackBufferWidth <- 1280
        game.Graphics.PreferredBackBufferHeight <- 720
        game.Graphics.IsFullScreen <- false

        game.Graphics.ApplyChanges ())

  game.Window.ClientSizeChanged.AddHandler (clientSizeHandler)

  game.Run ()

  0
