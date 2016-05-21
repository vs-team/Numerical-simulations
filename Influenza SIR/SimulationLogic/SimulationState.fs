module SimulationState

open Time
open Coroutine
open CoScheduler
open RandomDistributions
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

type InfectionStatus = Susceptible | Infected | Immune | Dead
type Gender          = Male | Female

type Id = uint32

let mutable uid = 0u
let getUId() = // TODO[multithreading hazard]
  uid <- uid + 1u
  uid - 1u  

type City =
  {
    Id       : Id
    Position : Vector2
    Agents   : System.Collections.Generic.Dictionary<Id,Agent> // TODO[multithreading hazard]
  }

and SimState = 
  {
    Cities            : City[]
    Agents            : Map<Id,Agent>
    Logic             : CoScheduler<SimState,Map<Id,Agent>>
    LastUpdateTime    : Time
    DefaultDeltaTime  : Time
  }

and Agent =
  {
    Id              : Id
    OffsetPosition  : Vector2
    City            : City
    Infection       : InfectionStatus
    Gender          : Gender
  }

let mutable SpawnedThreads : List<Co<SimState,Map<Id,Agent>,Unit>> = [] // TODO[mutability hazard]

let self : Accessor<SimState,Agent,Agent> = { Getter = getSt; Setter = setSt }
let agentId : Property<Agent,Id> = { Get = (fun a -> a.Id); Set = (fun a x -> { a with Id = x }) }
let city : Property<Agent,City> = { Get = (fun a -> a.City); Set = (fun a x -> { a with City = x }) }
let infection : Property<Agent,InfectionStatus> = { Get = (fun a -> a.Infection); Set = (fun a x -> { a with Infection = x }) }
let agents : Property<City,System.Collections.Generic.Dictionary<Id,Agent>> = { Get = (fun a -> a.Agents); Set = (fun a x -> { a with Agents = x }) }
let isDead : Property<InfectionStatus,bool> = { Get = (fun a -> a = Dead); Set = (fun a x -> a) }

