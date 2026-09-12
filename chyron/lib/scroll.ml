open Core

type direction = Left | Right [@@deriving sexp]
type mode = Reset | Wrap [@@deriving sexp]

type t = {
  cycles : int;
  direction : direction;
  endcap_char : char;
  endcap_len : int;
  rest : int;
  mode : mode;
  step : Sb.Step.t;
  sleep : int;
}

let direction_arg =
  Command.Arg_type.of_alist_exn ~accept_unique_prefixes:true
    ~case_sensitive:false ~list_values_in_help:false
    [ ("left", Left); ("right", Right) ]

let mode_arg =
  Command.Arg_type.of_alist_exn ~accept_unique_prefixes:true
    ~case_sensitive:false ~list_values_in_help:false
    [ ("reset", Reset); ("wrap", Wrap) ]

let run_scroll text Universal.{ prefix; suffix; terminator; width }
    { cycles; direction; endcap_char; endcap_len; rest; mode; step; sleep } =
  let joined_bytes, jointextlen, visual_chars = Sb.sbvals text in
  let ecl =
    Int.clamp_exn
      (Int.max endcap_len (width - visual_chars))
      ~min:1 ~max:(pred width)
  in
  let ecp = Bytes.make ecl endcap_char in
  let base = Stdlib.Bytes.cat (Stdlib.Bytes.cat ecp joined_bytes) ecp in
  let finaltext =
    match direction with
    | Left -> Stdlib.Bytes.cat joined_bytes base
    | Right -> Stdlib.Bytes.cat base joined_bytes
  in
  let ltfunc pwlist pwlen =
    let tot = sleep + rest in
    match mode with
    | Wrap ->
        List.mapi pwlist ~f:(fun i (p, w) ->
            if i = 0 then (p, w, tot) else (p, w, sleep))
    | Reset ->
        List.mapi pwlist ~f:(fun i (p, w) ->
            if i = 0 || i = pred pwlen then (p, w, tot) else (p, w, sleep))
  in
  let ( loopandprint,
        term,
        lenminuswidth,
        halflen,
        brword,
        blchar,
        blword,
        rchar,
        takeappend,
        _ ) =
    Sb.sbfuncs ltfunc finaltext
      Universal.{ prefix; suffix; terminator; width }
      cycles
  in
  begin
    let open Sb.Step in
    match
      (direction, step, mode, Ordering.of_int (compare visual_chars width))
    with
    | Left, Char, Reset, Greater ->
        blchar
        |> List.filter ~f:(fun (a, b) -> a + b <= halflen - ecl)
        |> loopandprint
    | Left, Char, Wrap, (Greater | Equal | Less) ->
        blchar |> List.filter ~f:(fun (a, _) -> a < halflen) |> loopandprint
    | Left, (Char | Word), Reset, Equal -> [ (0, jointextlen) ] |> loopandprint
    | Left, Word, Reset, Greater -> begin
        let l, r =
          List.split_while blword ~f:(fun (a, b) -> a + b < halflen - ecl)
        in
        takeappend r l |> loopandprint
      end
    | Left, Word, Wrap, (Greater | Equal | Less) ->
        List.take blword (List.length text) |> loopandprint
    | (Left | Right), (Char | Word), Reset, Less ->
        [ (0, jointextlen + ecl) ] |> loopandprint
    | Right, Char, Reset, Greater ->
        rchar
        |> List.filter ~f:(fun (a, _) ->
            a >= halflen + ecl && a < succ lenminuswidth)
        |> loopandprint
    | Right, Char, Wrap, (Greater | Equal | Less) ->
        rchar
        |> List.filter ~f:(fun (a, _) ->
            a > lenminuswidth - halflen && a < succ lenminuswidth)
        |> loopandprint
    | Right, (Char | Word), Reset, Equal ->
        [ (ecl, jointextlen) ] |> loopandprint
    | Right, Word, Reset, Greater -> begin
        let l, r =
          List.split_while brword ~f:(fun (a, _) -> a > halflen + ecl)
        in
        takeappend r l |> loopandprint
      end
    | Right, Word, Wrap, (Greater | Equal | Less) ->
        List.take brword (List.length text) |> loopandprint
  end;
  term
