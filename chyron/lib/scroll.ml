open Core

module Direction = struct
  type t = Left | Right [@@deriving enumerate, sexp]
end

module Mode = struct
  type t = Reset | Wrap [@@deriving enumerate, sexp]
end

let direction_arg =
  Command.Arg_type.enumerated_sexpable ~accept_unique_prefixes:true
    ~case_sensitive:false ~list_values_in_help:true
    (module Direction : Command.Enumerable_sexpable with type t = Direction.t)

let mode_arg =
  Command.Arg_type.enumerated_sexpable ~accept_unique_prefixes:true
    ~case_sensitive:false ~list_values_in_help:true
    (module Mode : Command.Enumerable_sexpable with type t = Mode.t)

type t = {
  cycles : int;
  direction : Direction.t;
  endcap_char : char;
  endcap_len : int;
  mode : Mode.t;
  rest : int;
  sleep : int;
  step : Sb.Step.t;
}

let run_scroll text Universal.{ prefix; suffix; terminator; visual; width }
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
    let open Direction in
    match direction with
    | Left -> Stdlib.Bytes.cat joined_bytes base
    | Right -> Stdlib.Bytes.cat base joined_bytes
  in
  let ltfunc pwlist pwlen =
    let tot = sleep + rest in
    let open Mode in
    match mode with
    | Wrap ->
        List.mapi pwlist ~f:(fun i (p, w) ->
            if i = 0 then (p, w, tot) else (p, w, sleep))
    | Reset ->
        List.mapi pwlist ~f:(fun i (p, w) ->
            if i = 0 || i = pred pwlen then (p, w, tot) else (p, w, sleep))
  in
  let ( loopandprint,
        lenminuswidth,
        halflen,
        brword,
        blchar,
        blword,
        rchar,
        takeappend,
        _ ) =
    Sb.sbfuncs ltfunc finaltext
      Universal.{ prefix; suffix; terminator; visual; width }
      cycles
  in
  begin
    let open Direction in
    let open Mode in
    let open Sb.Step in
    match
      (direction, step, mode, Ordering.of_int (compare visual_chars width))
    with
    | Left, Char, Reset, Greater ->
        true |> blchar
        |> List.filter ~f:(fun (a, b) -> a + b <= halflen - ecl)
        |> loopandprint
    | Left, Char, Wrap, (Greater | Equal | Less) ->
        (* print_endline
        @@ List.to_string
             ~f:(fun (a, b) -> string_of_int a ^ " " ^ string_of_int b)
             (blchar true); *)
        true |> blchar
        |> List.filter ~f:(fun (a, _) -> a < halflen)
        |> loopandprint
    | Left, (Char | Word), Reset, Equal -> [ (0, jointextlen) ] |> loopandprint
    | Left, Word, Reset, Greater -> begin
        let l, r =
          true |> blword
          |> List.split_while ~f:(fun (a, b) -> a + b < halflen - ecl)
        in
        takeappend r l |> loopandprint
      end
    | Left, Word, Wrap, (Greater | Equal | Less) ->
        List.take (blword true) (List.length text) |> loopandprint
    | (Left | Right), (Char | Word), Reset, Less ->
        [ (0, jointextlen + ecl) ] |> loopandprint
    | Right, Char, Reset, Greater ->
        false |> rchar
        |> List.filter ~f:(fun (a, _) ->
            a >= halflen + ecl && a < succ lenminuswidth)
        |> loopandprint
    | Right, Char, Wrap, (Greater | Equal | Less) ->
        let lenminuswidth = 26 in
        print_endline @@ string_of_int lenminuswidth;
        print_endline @@ string_of_int halflen;
        print_endline
        @@ List.to_string
             ~f:(fun (a, b) -> string_of_int a ^ " " ^ string_of_int b)
             (rchar false);

        false |> rchar
        |> List.filter ~f:(fun (a, _) ->
            a > lenminuswidth - halflen && a < succ lenminuswidth)
        |> loopandprint
    | Right, (Char | Word), Reset, Equal ->
        [ (ecl, jointextlen) ] |> loopandprint
    | Right, Word, Reset, Greater -> begin
        let l, r =
          true |> brword
          |> List.split_while ~f:(fun (a, _) -> a > halflen + ecl)
        in
        takeappend r l |> loopandprint
      end
    | Right, Word, Wrap, (Greater | Equal | Less) ->
        List.take (brword true) (List.length text) |> loopandprint
  end
