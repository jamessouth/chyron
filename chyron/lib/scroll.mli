module Direction : sig
  type t = Left | Right

  val sexp_of_t : t -> Sexplib0.Sexp.t
end

module Mode : sig
  type t = Reset | Wrap

  val sexp_of_t : t -> Sexplib0.Sexp.t
end

val direction_arg : Direction.t Command.Arg_type.t
val mode_arg : Mode.t Command.Arg_type.t

type t = {
  cycles : int;
  direction : Direction.t;
  endcap_char : char;
  endcap_len : int;
  mode : Mode.t;
  rest : int;
  sleep : int;
  step : Sb.Step.t;
}

val run_scroll : string list -> Universal.t -> t -> unit
