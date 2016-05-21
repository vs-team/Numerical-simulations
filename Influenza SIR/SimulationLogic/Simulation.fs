module Simulation

open Time
open Coroutine
open CoScheduler
open RandomDistributions
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System.Collections.Generic
open SimulationState
open AgentSimulation

let setupState 
  cityPositions numAgents 
  expectedDeathAgeInYears expectedTimeBetweenContactsInHours expectedTravelTimeBetweenCitiesInDays 
  initialInfectionProbability
  defaultDeltaTime =
  let cities =
    [|
      for cp in cityPositions do
      yield 
        {
          Id        = getUId()
          Position  = cp
          Agents    = Dictionary()
        }
    |]
  let agents =
    [
      for i = 1 to numAgents do
      let id = getUId()
      yield id,
        {
          Id              = id
          OffsetPosition  = Vector2(r.Next(-128,128) |> float32, r.Next(-128,128) |> float32)
          City            = chooseAny cities
          Infection       = if r.NextDouble() > initialInfectionProbability then InfectionStatus.Susceptible 
                            else InfectionStatus.Infected
          Gender          = chooseAny [| Male; Female |]
        }
    ] |> Map.ofList
  let logic = 
    {
      Active  = 
        [
          for x in agents do
          let id = x.Value.Id
          if x.Value.Infection = Infected then
            yield agent_from_id id runSickness
          yield agent_from_id id (birthToDeath expectedDeathAgeInYears)
          yield agent_from_id id (interact expectedTimeBetweenContactsInHours)
          yield agent_from_id id (travel expectedTravelTimeBetweenCitiesInDays)
        ]
      Waiting = []
    }
  {
    Cities            = cities
    Agents            = agents
    Logic             = logic
    LastUpdateTime    = 0.0<hour>
    DefaultDeltaTime  = defaultDeltaTime
  }

let updateState (state:SimState) =
  let logic,agents',current_time = updateScheduler state.Logic state state.Agents state.LastUpdateTime state.DefaultDeltaTime
  let spawned = SpawnedThreads
  do SpawnedThreads <- []
  {
    state with 
      Agents         = agents'
      Logic          = { logic with Active = spawned @ logic.Active }
      LastUpdateTime = current_time
  }
