val list_concat : sep:'a list -> 'a list list -> 'a list

module Charset : sig
  type t = Lowers | Uppers | Numbers | Symbols1 | Symbols2 | Distros

  val all : t list
  val t_of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val lowers : string list
  val uppers : string list
  val numbers : string list
  val symbols1 : string list
  val symbols2 : string list
  val distros : string list
  val chars : t list -> string list
end

module Justify : sig
  type t = Center | Left | Right

  val all : t list
  val t_of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
end

module Direction : sig
  type t = Ascending | Descending

  val all : t list
  val t_of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
end

type alpha = { direction : Direction.t }
type rando = { flip_hi_bound : int; flip_lo_bound : int }

val charset_arg : Charset.t list Command.Arg_type.t
val justify_arg : Justify.t Command.Arg_type.t
val direction_arg : Direction.t Command.Arg_type.t

type t = {
  charsets : Charset.t list;
  cycles : int;
  flip_sleep : int;
  justify : Justify.t;
  sleep : int;
}

val breakdown : int -> string list -> string list list
val buildup : int -> string list list -> string list list
val pad : int -> Justify.t -> string list list -> string list list
val extendchars : string list -> string list -> string list
val run_split_flap_rando : string list -> Universal.t -> t -> rando -> unit
val run_split_flap_alpha : string list -> Universal.t -> t -> alpha -> unit
