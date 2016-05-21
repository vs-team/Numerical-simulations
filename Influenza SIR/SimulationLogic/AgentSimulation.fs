module AgentSimulation

open Time
open Coroutine
open CoScheduler
open RandomDistributions
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System.Collections.Generic
open SimulationState

let birthToDeath (expectedDeathAge:float<year>) : Co<SimState,Agent,Unit> = 
  co{
    let ageOfDeath = timeToNextEvent expectedDeathAge
    let ageOfDeathInHours = timeToNextEvent expectedDeathAge |> y_to_w |> w_to_d |> d_to_h
    do! wait ageOfDeathInHours
    do! (self => infection) := InfectionStatus.Dead
  }

// TODO: each character should have an own distribution of travel expectations
let travel (expectedTravelTimeBetweenCities:float<day>) : Co<SimState,Agent,Unit> = 
  co{
    let! ctxt = getCtxt
    let timeToNextTrip = timeToNextEvent expectedTravelTimeBetweenCities
    do! wait (timeToNextTrip |> d_to_h)
    let nextCity = chooseAny ctxt.Cities
    let! prevCity = !(self => city)
    let! id = !(self => agentId)
    let! a = !self
    do prevCity.Agents.Remove(id) |> ignore
    do nextCity.Agents.Add(id,a)
    do! (self => city) := nextCity
  } |> repeat_until !(self => infection => isDead)

// TODO: chance of passing sickness?
let contactResults (is1:InfectionStatus) (is2:InfectionStatus) : InfectionStatus * InfectionStatus = 
  match is1,is2 with
  | Dead,_ -> Dead,is2
  | _,Dead -> is1,Dead
  | Susceptible,Infected -> Infected,Infected
  | Infected,Susceptible -> Infected,Infected
  | Immune,_ -> Immune,is2
  | _,Immune -> is1,Immune
  | _ -> is1,is2

let lub (is1:InfectionStatus) (is2:InfectionStatus) : InfectionStatus = 
  match is1,is2 with
  | Dead,_ -> Dead
  | _,Dead -> Dead
  | Immune,_ -> Immune
  | _,Immune -> Immune
  | _ -> failwithf "Cannot compare %A and %A" is1 is2

let runSickness : Co<SimState,Agent,Unit> = 
  co{
    let sicknessDuration = 4.0<day> |> d_to_h
    do! wait sicknessDuration
    let! currentInfectionStatus = !(self => infection)
    do! (self => infection) := lub Immune currentInfectionStatus
  }

let rec agent_from_id (id:Id) (c:Co<SimState,Agent,Unit>) : Co<SimState,Map<Id,Agent>,Unit> = 
  convert_state c { Get = Map.find id; Set = (fun agents agent -> agents |> Map.add id agent) }

let interact (expectedTimeBetweenContacts:float<hour>) : Co<SimState,Agent,Unit> = 
  co{
    let! ctxt = getCtxt
    let timeToNextContact = timeToNextEvent expectedTimeBetweenContacts
    do! wait timeToNextContact
    let! possibleTargets = !((self => city) => agents)
    if possibleTargets.Count > 0 then
      let other = chooseAnyFromDictionary possibleTargets
      let! prevInfectionStatus = !(self => infection)
      let newInfectionStatus,newInfectionStatusOther = contactResults prevInfectionStatus other.Infection
      do! (self => infection) := newInfectionStatus
      if prevInfectionStatus <> Infected && newInfectionStatus = Infected then
        let! id = !(self => agentId)
        do SpawnedThreads <- agent_from_id id runSickness :: SpawnedThreads // TODO[mutability hazard]
      // TODO: should both get sick?
  } |> repeat_until !(self => infection => isDead)
