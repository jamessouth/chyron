type t = {
  cycles : int;
  endcap_char : char;
  rest : int;
  step : Sb.Step.t;
  sleep : int;
}

val run_bounce : string list -> Universal.t -> t -> unit
