module Redwood.Tests.Logic

open Expecto
open Redwood
open Redwood.Logic

[<Tests>]
let tests =
  testList "logic" [
    test "Logic computation expression should work as expected 1" {
      let actual =
        logic {
          ()
        }
        |> Logic.run 123

      let expected = 123, []

      Expect.equal actual expected "no-op logic perserves state"
    }

    test "Logic computation expression should work as expected 2" {
      let actual =
        logic {
          let! state = Logic.getState
          do! Logic.setState (state + 1)
        }
        |> Logic.run 1

      let expected = 2, []

      Expect.equal actual expected "logic updates state"
    }

    test "Logic computation expression should work as expected 3" {
      let actual =
        logic {
          do! Logic.playSound "bird.wav"
        }
        |> Logic.run ()

      let expected = (), [ PlaySound "bird.wav" ]

      Expect.equal actual expected "logic emits actions"
    }

    test "Logic computation expression should work as expected 4" {
      let actual =
        logic {
          do! Logic.playSound "a.wav"
          do! Logic.playSound "b.wav"
          do! Logic.playSound "c.wav"
        }
        |> Logic.run ()

      let expected =
        (),
        [
          PlaySound "a.wav"
          PlaySound "b.wav"
          PlaySound "c.wav"
        ]

      Expect.equal actual expected "logic emits multiple actions"
    }

    test "Logic computation expression should work as expected 5" {
      let actual =
        logic {
          let sounds = [ "a.wav"; "b.wav"; "c.wav" ]

          for sound in sounds do
            do! Logic.playSound sound
        }
        |> Logic.run ()

      let expected =
        (),
        [
          PlaySound "a.wav"
          PlaySound "b.wav"
          PlaySound "c.wav"
        ]

      Expect.equal actual expected "logic supports loops"
    }
  ]
