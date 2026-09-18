module Charset : sig
  type t = Lowers | Uppers | Numbers | Symbols1 | Symbols2 | Distros

  val all : t list
  val sexp_of_t : t -> Sexplib0.Sexp.t
end

module Justify : sig
  type t = Center | Left | Right

  val sexp_of_t : t -> Sexplib0.Sexp.t
end

type alpha = { multiple : int }
type rando = { flip_hi_bound : int; flip_lo_bound : int }

val charset_arg : Charset.t list Command.Arg_type.t
val justify_arg : Justify.t Command.Arg_type.t

type t = {
  charsets : Charset.t list;
  cycles : int;
  flip_sleep : int;
  justify : Justify.t;
  sleep : int;
}

val run_split_flap_rando : string list -> Universal.t -> t -> rando -> unit
val run_split_flap_alpha : string list -> Universal.t -> t -> alpha -> unit
