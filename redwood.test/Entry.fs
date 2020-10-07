open Expecto
open Expecto.Impl
open Redwood.Tests

[<Tests>]
let allTests =
  testList "all-tests" [
    Logic.tests
  ]

[<EntryPoint>]
let main argv =
  runTestsWithArgs ExpectoConfig.defaultConfig argv allTests
