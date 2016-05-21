module RandomDistributions

let r = System.Random()

let timeToNextEvent (average_time_to_event:float< 'a >)  =
  let λ = 1.0<_> / average_time_to_event
  let u = (float(r.Next(0, 10000))) / 10000.0
  (log(1.0-u) / (-λ)) |> float |> ceil |> LanguagePrimitives.FloatWithMeasure<'a>

let chooseAny (x:'a[]) =
  let u = r.Next(x.Length)
  x.[u]

let chooseAnyFromDictionary (x:System.Collections.Generic.Dictionary<'a,'b>) =
  let u = r.Next(x.Count)
  (x |> Seq.item u).Value
