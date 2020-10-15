namespace Redwood.Pathfinding

type Path<'node> = 'node list

type IGraph<'node, 'cost when 'node : equality and 'cost : comparison> =
  abstract member GetNeighbours : 'node -> seq<'node * 'cost>

[<RequireQualifiedAccess>]
module Graph =

  let ofConnections twoWay (connections : seq<'node * 'node * 'cost>) =
    // TODO: Optimize!
    let connections = Seq.toList connections

    {
      new IGraph<'node, 'cost> with
        member this.GetNeighbours (start : 'node) =
          connections
          |> Seq.collect
            (fun (x, y, c) ->
              seq {
                if x = start
                then
                  yield (y, c)

                if twoWay && y = start
                then
                  yield (x, c)
              })
    }

  let getNeighbours node (graph : IGraph<_, _>) =
    graph.GetNeighbours node

module Pathfinding =

  open FSharpx.Collections

  let inline tryAStar (heuristic : ^node -> ^node -> ^cost) (start : ^node) (goal : ^node) (graph : IGraph<_, _>) : Path<_> option =
    let isDescending = false

    let mutable frontier : IPriorityQueue<'cost * 'node> =
      PriorityQueue.empty isDescending
      |> PriorityQueue.insert
        (LanguagePrimitives.GenericZero, start)

    let mutable cameFrom : Map<'node, 'node * 'cost> =
      Map.empty

    while not (PriorityQueue.isEmpty frontier) do
      let (_, current), tail = PriorityQueue.pop frontier

      frontier <- tail

      if current = goal
      then
        frontier <- PriorityQueue.empty isDescending // Done
      else
        let costSoFar =
          match Map.tryFind current cameFrom with
          | Some (_, cost) -> cost
          | None -> LanguagePrimitives.GenericZero

        for next, cost in Graph.getNeighbours current graph do
          let newCost = costSoFar + cost

          match Map.tryFind next cameFrom with
          | Some (_, previousCost) ->
            if previousCost > newCost
            then
              let priority = newCost + (heuristic goal next)

              cameFrom <- cameFrom |> Map.add next (current, newCost)
              frontier <- frontier |> PriorityQueue.insert (priority, next)
          | None ->
            let priority = newCost + (heuristic goal next)

            cameFrom <- cameFrom |> Map.add next (current, newCost)
            frontier <- frontier |> PriorityQueue.insert (priority, next)

    let rec reconstructPath current path =
      let path = current :: path

      match Map.tryFind current cameFrom with
      | Some (origin, _) ->
        if origin = start
        then
          start :: path |> Some
        else
          reconstructPath origin path
      | None -> None

    reconstructPath goal []
