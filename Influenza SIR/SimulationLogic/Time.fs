module Time

[<Measure>]
type hour

[<Measure>]
type day

[<Measure>]
type week

[<Measure>]
type year

// We choose hours as the standard simulation time
type Time = float<hour>

// Forward transformations
let h_to_d (x:float<hour>) : float<day> = x / 24.0<hour/day>
let d_to_w (x:float<day>) : float<week> = x / 7.0<day/week>
let w_to_y (x:float<week>) : float<year> = x / 52.0<week/year>
let h_to_y = h_to_d >> d_to_w >> w_to_y

// Inverse transformations
let y_to_w (x:float<year>) = x * 52.0<week/year>
let w_to_d (x:float<week>) = x * 7.0<day/week>
let d_to_h (x:float<day>) = x * 24.0<hour/day>
let y_to_h = y_to_w >> w_to_d >> d_to_h
