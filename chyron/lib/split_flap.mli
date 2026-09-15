module Charset : sig
  type t = Lowers | Uppers | Numbers | Symbols1 | Symbols2 | Distros

  val all : t list
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val arg : t list Command.Arg_type.t
end

type justify = Center | Left | Right

val sexp_of_justify : justify -> Sexplib0.Sexp.t
val justify_arg : justify Command.Arg_type.t

type t = {
  charsets : Charset.t list;
  cycles : int;
  flip_hi_bound : int;
  flip_lo_bound : int;
  flip_sleep : int;
  justify : justify;
  sleep : int;
}

val run_split_flap : string list -> Universal.t -> t -> unit
