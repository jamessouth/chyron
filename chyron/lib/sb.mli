module Step : sig
  type t = Char | Word

  val sexp_of_t : t -> Sexplib0.Sexp.t
end

type sb_values = {
  joined_bytes : bytes;
  joined_text_len : int;
  unicode_char_list : string list;
  visual_chars : int;
  visual_columns : int;
}

val step_arg : Step.t Command.Arg_type.t
val bytesofutfchars : string -> int -> int
val sbvals : string list -> sb_values

val ucinds :
  bool ->
  string list ->
  (string list -> int) ->
  (string -> int -> int -> 'a list -> 'a list) ->
  int ->
  'a list

val wordsplitfn : string list -> int
val accmfn : 'a -> 'b -> 'c -> ('c * 'b) list -> ('c * 'b) list
val takeappend : 'a list -> 'a list -> 'a list

val loopandprint :
  ('a list -> (int * int * int) list) ->
  ('b -> int -> int -> unit) ->
  (unit -> 'c) ->
  'b ->
  int ->
  'a list ->
  'c

val doublewidth :
  (string * int * int) list ->
  (string * int * int) list ->
  (string list -> string list) ->
  int ->
  int ->
  (bytes -> int -> int -> unit) ->
  (unit -> 'a) ->
  int ->
  (string ->
  int ->
  int ->
  int ->
  (string * int * int) list ->
  (string * int * int) list * int) ->
  int ->
  'a
