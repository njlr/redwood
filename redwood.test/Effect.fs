module Redwood.Tests.Effect

open Expecto
open Redwood
open Redwood.Effect

[<Tests>]
let tests =
  testList "effect" [
    test "Effect computation expression should work as expected 1" {
      let actual =
        effect {
          ()
        }
        |> Effect.run 123

      let expected = 123, []

      Expect.equal actual expected "no-op effect"
    }
  ]
