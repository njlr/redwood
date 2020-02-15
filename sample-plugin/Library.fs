namespace HotReload.SamplePlugin

open HotReload

type SamplePlugin () =
  interface IPlugin with
    member this.Name
      with get () = "Sample3"
    member this.Update dt state =
      state
