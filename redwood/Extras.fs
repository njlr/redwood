namespace Redwood.Extras

module Option =

  open Aether

  let valueOrDefault (fallback : 't) : Lens<'t option, 't> =
    (fun x -> x |> Option.defaultValue fallback), (fun v _ -> Some v)
