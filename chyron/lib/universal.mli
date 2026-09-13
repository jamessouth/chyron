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

type terminator = Newline | Return | Space

val sexp_of_terminator : terminator -> Sexplib0.Sexp.t

type t = {
  prefix : string;
  suffix : string;
  terminator : terminator;
  width : int;
}

val terminator_arg : terminator Command.Arg_type.t
val printandterm : t -> (bytes -> int -> int -> unit) * (unit -> unit)
val uc_charlist : string -> string list
