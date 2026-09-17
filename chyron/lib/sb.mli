module Step : sig
  type t = Char | Word

  val sexp_of_t : t -> Sexplib0.Sexp.t
end

val step_arg : Step.t Command.Arg_type.t
val sbvals : string list -> bytes * int * int

val sbfuncs :
  ('a list -> int -> (int * int * int) list) ->
  bytes ->
  Universal.t ->
  int ->
  ('a list -> unit)
  * int
  * int
  * (int * int) list
  * (int * int) list
  * (int * int) list
  * (int * int) list
  * ('b list -> 'b list -> 'b list)
  * int
