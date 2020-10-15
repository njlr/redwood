module Redwood.Tests.Linear

open Expecto
open Redwood.Linear

[<Tests>]
let tests =
  testList "linear" [
    test "Linear computation works as expected 1" {
      let actual =
        (Vector2.create 1.0f 2.0f) + (Vector2.create 3.0f 4.0f)

      let expected = Vector2.create 4.0f 6.0f

      Expect.equal actual expected "simple addition"
    }

    test "Linear computation works as expected 2" {
      let actual =
        (Vector2.create 1.0f 2.0f) * 200.0f

      let expected = Vector2.create 200.0f 400.0f

      Expect.equal actual expected "simple multiplication"
    }

    test "Linear computation works as expected 3" {
      let actual =
        (Vector2.create 3.0f 4.0f)
        |> Vector2.length

      let expected = 5.0f

      Expect.equal actual expected "length calculation"
    }
  ]
