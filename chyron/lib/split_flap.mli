module Charset : sig
  type t =
    | Lowers
    | Uppers
    | Numbers
    | Symbols1
    | Symbols2
    | Distros
    | PLs
    | Chars_in_TEXT

  val all : t list
  val sexp_of_t : t -> Sexplib0.Sexp.t
end

module Justify : sig
  type t = Center | Left | Right

  val sexp_of_t : t -> Sexplib0.Sexp.t
end

module Direction : sig
  type t = Down | Up

  val sexp_of_t : t -> Sexplib0.Sexp.t
end

type alpha = { direction : Direction.t }
type rando = { flip_hi_bound : int; flip_lo_bound : int }

val charset_arg : Charset.t list Command.Arg_type.t
val justify_arg : Justify.t Command.Arg_type.t
val direction_arg : Direction.t Command.Arg_type.t

type t = {
  charsets : Charset.t list;
  custom_chars : string;
  cycles : int;
  flip_sleep : int;
  justify : Justify.t;
  sleep : int;
}

val run_split_flap_rando : string list -> Universal.t -> t -> rando -> unit
val run_split_flap_alpha : string list -> Universal.t -> t -> alpha -> unit
