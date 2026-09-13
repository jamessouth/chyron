type direction = Left | Right

val sexp_of_direction : direction -> Sexplib0.Sexp.t

type mode = Reset | Wrap

val sexp_of_mode : mode -> Sexplib0.Sexp.t

type t = {
  cycles : int;
  direction : direction;
  endcap_char : char;
  endcap_len : int;
  mode : mode;
  rest : int;
  sleep : int;
  step : Sb.Step.t;
}

val direction_arg : direction Command.Arg_type.t
val mode_arg : mode Command.Arg_type.t
val run_scroll : string list -> Universal.t -> t -> unit
