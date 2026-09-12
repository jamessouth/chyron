open Core

module Externs = struct
  external caml_clock_nanosleep : int -> unit = "caml_clock_nanosleep"
  [@@noalloc]

  external unsafe_output_bytes : Out_channel.t -> bytes -> int -> int -> unit
    = "caml_ml_output_bytes"
  [@@noalloc]

  external unsafe_output_char : Out_channel.t -> char -> unit
    = "caml_ml_output_char"
  [@@noalloc]

  external unsafe_flush : Out_channel.t -> unit = "caml_ml_flush" [@@noalloc]
end

type terminator = Newline | Return | Space [@@deriving sexp]

type t = {
  prefix : string;
  suffix : string;
  terminator : terminator;
  width : int;
}

let terminator_arg =
  Command.Arg_type.of_alist_exn ~accept_unique_prefixes:true
    ~case_sensitive:false ~list_values_in_help:false
    [ ("newline", Newline); ("return", Return); ("space", Space) ]

let printandterm { prefix; suffix; terminator; _ } =
  (* setting these chars here instead of as constructor payloads because of the way they make the help text look*)
  let pfix = Bytes.of_string prefix in
  let sfix = Bytes.of_string suffix in
  let print ft pos wid =
    Externs.unsafe_output_bytes stdout pfix 0 (Bytes.length pfix);
    Externs.unsafe_output_bytes stdout ft pos wid;
    Externs.unsafe_output_bytes stdout sfix 0 (Bytes.length sfix);
    Externs.unsafe_output_char stdout
      (match terminator with Newline -> '\n' | Return -> '\r' | Space -> ' ');
    Externs.unsafe_flush stdout
  in
  let term =
    match terminator with
    | Newline -> ()
    | Return | Space ->
        Externs.unsafe_output_char stdout '\n';
        Externs.unsafe_flush stdout
  in
  (print, term)

let uc_charlist str =
  Uuseg_string.fold_utf_8 `Grapheme_cluster (fun acc char -> char :: acc) [] str
