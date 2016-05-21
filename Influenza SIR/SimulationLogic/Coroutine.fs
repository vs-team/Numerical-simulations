module Coroutine

open Time

type Co<'ctxt,'s,'a> = 'ctxt -> 's -> Time -> StepRes<'ctxt,'s,'a>
and StepRes<'ctxt,'s,'a> = 
  | Done of 's * 'a 
  | Paused of 's * Co<'ctxt,'s,'a> 
  | Waiting of 's * Time * Co<'ctxt,'s,'a>

let getCtxt = fun ctxt s t -> Done(s,ctxt)
let getSt = fun ctxt s t -> Done(s,s)
let setSt s' = fun ctxt s t -> Done(s',())  

let ret x = fun ctxt s t -> Done(s,x)
let yie = fun ctxt s t -> Paused(s, ret())
let wait dt = fun ctxt s t -> Waiting(s, t + dt, ret())
let rec (>>=) (p:Co<'ctxt,'s,'a>) (k:'a->Co<'ctxt,'s,'b>) : Co<'ctxt,'s,'b> =
  fun ctxt s t ->
    match p ctxt s t with
    | Done(s',x) -> k x ctxt s' t
    | Paused(s',p') -> Paused(s',p' >>= k)
    | Waiting(s',time_to_reach,p') ->
      Waiting(s',time_to_reach,p' >>= k)

type CoBuilder() =
  member this.Return(x) = ret x
  member this.Bind(p,k) = p >>= k
  member this.Combine(p,k) = p >>= (fun () -> k)
  member this.ReturnFrom(c) = c
  member this.Zero() = ret()
  member this.Delay(p) = p()
let co = CoBuilder()

let rec repeat c = 
  co{
    let! _ = c
    return! repeat c
  }  

let rec repeat_until t c = 
  co{
    let! b = t
    if not b then
      do! c
      return! repeat_until t c
  }  

type Accessor<'ctxt,'s,'a> = { Getter : Co<'ctxt,'s,'a>; Setter : 'a -> Co<'ctxt,'s,Unit> }
type Property<'a,'b> = { Get : 'a -> 'b; Set : 'a -> 'b -> 'a }
let (!) (a:Accessor<'ctxt,'s,'a>) : Co<'ctxt,'s,'a> = a.Getter
let (:=) (a:Accessor<'ctxt,'s,'a>) (x:'a) : Co<'ctxt,'s,Unit> = a.Setter x
let (=>) (a:Accessor<'ctxt,'s,'a>) (f:Property<'a,'b>) : Accessor<'ctxt,'s,'b> = 
  {
    Getter = 
      co{
        let! s = a.Getter
        return f.Get s
      }
    Setter = 
      fun v ->
        co{
          let! s = a.Getter
          let s' = f.Set s v
          return! a.Setter s'
        }
  }

let rec convert_state (c:Co<'ctxt,'q,'a>) (p:Property<'s,'q>) : Co<'ctxt,'s,'a> = 
  fun ctxt s t -> 
    let q = s |> p.Get
    match c ctxt q t with 
    | Done(q',x) ->
      Done(p.Set s q',x)
    | Paused(q',k) ->
      Paused(p.Set s q',convert_state k p)
    | Waiting(q',target_time,k) ->
      Waiting(p.Set s q',target_time,convert_state k p)

