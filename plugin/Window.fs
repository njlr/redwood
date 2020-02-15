module HotReload.Sdl

open System
open System.Runtime.InteropServices
open SDL2

type Window private (windowPtr : IntPtr) =
  let mutable windowPtr = windowPtr
  interface IDisposable with
    member this.Dispose () =
      if windowPtr <> IntPtr.Zero
      then
        SDL.SDL_DestroyWindow windowPtr
        windowPtr <- IntPtr.Zero
      ()

  with
    member this.Show () =
      if windowPtr <> IntPtr.Zero
      then
        SDL_ShowWindow windowPtr

    static member TryCreate () =
      let windowPtr = SDL.SDL_CreateWindow("Redwood", 0, 0, 640, 480, SDL_WindowFlags.SDL_WINDOW_HIDDEN)

      if windowPtr = IntPtr.Zero
      then
        let errorMessage = SDL.SDL_GetError ()
        Result.Error errorMessage
      else
        Result.Ok (Window windowPtr)
