module Externs : sig
  external caml_clock_nanosleep : int -> unit = "caml_clock_nanosleep"
  [@@noalloc]

  external unsafe_output_bytes : out_channel -> bytes -> int -> int -> unit
    = "caml_ml_output_bytes"
  [@@noalloc]

  external unsafe_output_char : out_channel -> char -> unit
    = "caml_ml_output_char"
  [@@noalloc]

  external unsafe_flush : out_channel -> unit = "caml_ml_flush" [@@noalloc]
end

module Terminator : sig
  type t = LF | CR | Space

  val sexp_of_t : t -> Sexplib0.Sexp.t
end

val terminator_arg : Terminator.t Command.Arg_type.t

type t = {
  prefix : string;
  suffix : string;
  terminator : Terminator.t;
  width : int;
}

val printandterm : t -> (bytes -> int -> int -> unit) * (unit -> unit)
val uc_charlist : string -> string list
