module CoScheduler

open Time
open Coroutine

type CoScheduler<'ctxt,'s> = 
  {
    Active  : List<Co<'ctxt,'s,Unit>>
    Waiting : List<Time * Co<'ctxt,'s,Unit>>
  }

let rec stepMany (l:List<Co<'ctxt,'s,Unit>>) (ctxt:'ctxt) (s:'s) (t:Time) =
  match l with
  | [] -> [],[],s
  | c::cs ->
    match c ctxt s t with
    | Done(s',()) ->
      stepMany cs ctxt s' t
    | Paused(s',k) ->
      let a,p,s'' = 
        stepMany cs ctxt s' t
      k::a,p,s''
    | Waiting(s',time_to_reach,k) ->
      let a,p,s'' = 
        stepMany cs ctxt s' t
      a,(time_to_reach,k)::p,s''

let rec insertSorted (time_to_reach:Time,k:Co<'ctxt,'s,Unit>) (l:List<Time * Co<'ctxt,'s,Unit>>) =
  match l with
  | [] -> [time_to_reach,k]
  | (time_to_reach',x)::xs when time_to_reach < time_to_reach' -> 
    (time_to_reach,k) :: l
  | (time_to_reach',x)::xs -> 
    (time_to_reach',x)::(insertSorted (time_to_reach,k) xs)

let rec wakeUp (l:List<Time * Co<'ctxt,'s,Unit>>) (ctxt:'ctxt) (s:'s) (t:Time) =
  match l with
  | [] -> [],[],s
  | (time_to_reach,c)::cs when t >= time_to_reach ->
    match c ctxt s time_to_reach with
    | Done(s',()) ->
      wakeUp cs ctxt s' t
    | Paused(s',k) ->
      let a,w,s'' = 
        wakeUp cs ctxt s' t
      k::a,w,s''
    | Waiting(s',time_to_reach',k) ->
      let a,w,s'' = 
        wakeUp cs ctxt s' t
      a,insertSorted (time_to_reach',k) w,s''
  | cs ->
    [],cs,s

let rec merge (l1:List<Time * Co<'ctxt,'s,Unit>>) (l2:List<Time * Co<'ctxt,'s,Unit>>) =
  match l1, l2 with
  | [],l -> l
  | l,[] -> l
  | (t_x,x)::xs, (t_y,y)::ys when t_x < t_y ->
    (t_x,x) :: merge xs l2
  | (t_x,x)::xs, (t_y,y)::ys->
    (t_y,y) :: merge l1 ys    

let updateScheduler (cs:CoScheduler<'ctxt,'s>) (ctxt:'ctxt) (s:'s) (lastUpdateTime:Time) (defaultDeltaTime:Time) =
  let t = 
    match cs.Waiting with
    | [] -> lastUpdateTime + defaultDeltaTime
    | (t',x)::xs -> 
      max t' (lastUpdateTime + defaultDeltaTime)
  let a_to_a, a_to_w, s' = 
    stepMany cs.Active ctxt s t 
  let a_to_w_by_t = a_to_w |> List.sortBy fst
  let w_to_a,w_to_w_by_t, s'' = 
    wakeUp cs.Waiting ctxt s' t
  let new_a = a_to_a @ w_to_a
  let new_w = merge a_to_w_by_t w_to_w_by_t
  { Active = new_a; Waiting = new_w }, s'', t
