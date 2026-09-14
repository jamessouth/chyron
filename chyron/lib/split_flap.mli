type justify = Center | Left | Right

val sexp_of_justify : justify -> Sexplib0.Sexp.t

type charset = Lowers | Uppers | Numbers | Symbols1 | Symbols2

val sexp_of_charset : charset -> Sexplib0.Sexp.t

type t = {
  charsets : charset list;
  cycles : int;
  flip_hi_bound : int;
  flip_lo_bound : int;
  flip_sleep : int;
  justify : justify;
  sleep : int;
}

val justify_arg : justify Command.Arg_type.t
val charset_arg : charset list Command.Arg_type.t
val run_split_flap : string list -> Universal.t -> t -> unit
