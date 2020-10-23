module Redwood.EntitySystem

open System
open Redwood
open Redwood.Logic

[<Struct>]
type EntityID = private | EntityID of Guid

[<Struct>]
type EntityAction<'tstate, 'teffect> =
  | SpawnEntity of EntityID * 'tstate
  | DestroyEntity of entityToDestroy : EntityID
  | Effect of 'teffect

type EntitySystem<'tstate> = Map<EntityID, 'tstate>

and [<Struct>] EntityContext<'tstate, 'teffect> =
  {
    Self : EntityID
    World : EntitySystem<'tstate>
  }

type EntityUpdate<'tstate, 'teffect> = EntityContext<'tstate, 'teffect> -> Logic<'tstate, 'tstate, Unit, EntityAction<'tstate, 'teffect>>

[<RequireQualifiedAccess>]
module Entity =

  let tryFindEntity id (world : EntitySystem<_>) =
    Map.tryFind id world

  let destroyEntity (id : EntityID) =
    Logic.emit (DestroyEntity id)

[<RequireQualifiedAccess>]
module EntitySystem =

  let empty : Map<EntityID, _> = Map.empty

  let createEntity initialState (entitySystem : EntitySystem<_>) : EntityID * EntitySystem<_> =
    let id = Guid.NewGuid () |> EntityID
    id, Map.add id initialState entitySystem

module Logic =

  [<RequireQualifiedAccess>]
  module Entity =

    let spawnEntity initialState =
      logic {
        let id = Guid.NewGuid () |> EntityID

        do! Logic.emit (SpawnEntity (id, initialState))

        return id
      }

  [<RequireQualifiedAccess>]
  module EntitySystem =

    let spawnEntity initialState =
      logic {
        let! state = Logic.getState

        let id, state = EntitySystem.createEntity initialState state

        do! Logic.setState state

        return id
      }

    let updateEntities (updateFn : EntityUpdate<'tstate, 'teffect>) : Logic<EntitySystem<'tstate>, EntitySystem<'tstate>, Unit, 'teffect> =
      logic {
        let! state = Logic.getState

        for entityID, entity in state |> Map.toSeq do
          let context =
            {
              Self = entityID
              World = state
            }

          let (nextEntity, effects), () = updateFn context entity

          do! Logic.mapState (Map.add entityID nextEntity)

          for effect in effects do
            match effect with
            | SpawnEntity (id, initialState) ->
              do! Logic.mapState (Map.add id initialState)
            | DestroyEntity entityID ->
              do! Logic.mapState (Map.remove entityID)
            | Effect effect ->
              do! Logic.emit effect
      }
