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
        doublewidth,
        charlist,
        revcharlist,
        _ ) =
    Sb.sbfuncs ltfunc finaltext
      Universal.{ prefix; suffix; terminator; visual; width }
      cycles
  in
  begin
    let open Direction in
    let open Mode in
    let open Sb.Step in
    let open Universal.Visual in
    match
      ( direction,
        step,
        mode,
        visual,
        Ordering.of_int (compare visual_chars width) )
    with
    | Left, Char, Reset, Chars, Greater ->
        true |> blchar
        |> List.filter ~f:(fun (a, b) -> a + b <= halflen - ecl)
        |> loopandprint
    | Left, Char, Reset, Columns, Greater -> ()
    | Left, Char, Wrap, Chars, (Greater | Equal | Less) ->
        true |> blchar
        |> List.filter ~f:(fun (a, _) -> a < halflen)
        |> loopandprint
    | Left, Char, Wrap, Columns, (Greater | Equal | Less) ->
        print_endline "here3";
        let xx =
          List.map revcharlist ~f:(fun x ->
              let a =
                String.Utf8.get (String.Utf8.of_string_unchecked x) ~byte_pos:0
              in
              let b = Uucp.Break.east_asian_width a in
              let c = match b with `W | `F -> 2 | _ -> 1 in
              (x, c))
        in
        doublewidth xx width true
    | Left, (Char | Word), Reset, Chars, Equal ->
        [ (0, jointextlen) ] |> loopandprint
    | Left, (Char | Word), Reset, Columns, Equal -> ()
    | Left, Word, Reset, Chars, Greater -> begin
        let l, r =
          true |> blword
          |> List.split_while ~f:(fun (a, b) -> a + b < halflen - ecl)
        in
        takeappend r l |> loopandprint
      end
    | Left, Word, Reset, Columns, Greater -> ()
    | Left, Word, Wrap, Chars, (Greater | Equal | Less) ->
        List.take (blword true) (List.length text) |> loopandprint
    | Left, Word, Wrap, Columns, (Greater | Equal | Less) -> ()
    | (Left | Right), (Char | Word), Reset, Chars, Less ->
        [ (0, jointextlen + ecl) ] |> loopandprint
    | (Left | Right), (Char | Word), Reset, Columns, Less -> ()
    | Right, Char, Reset, Chars, Greater ->
        false |> rchar
        |> List.filter ~f:(fun (a, _) ->
            a >= halflen + ecl && a < succ lenminuswidth)
        |> loopandprint
    | Right, Char, Reset, Columns, Greater -> ()
    | Right, Char, Wrap, Chars, (Greater | Equal | Less) ->
        print_endline "here1";
        false |> rchar
        |> List.filter ~f:(fun (a, _) ->
            a > lenminuswidth - halflen && a < succ lenminuswidth)
        |> loopandprint
    | Right, Char, Wrap, Columns, (Greater | Equal | Less) ->
        print_endline "here2";
        let xx =
          List.map charlist ~f:(fun x ->
              let a =
                String.Utf8.get (String.Utf8.of_string_unchecked x) ~byte_pos:0
              in
              let b = Uucp.Break.east_asian_width a in
              let c = match b with `W | `F -> 2 | _ -> 1 in
              (x, c))
        in
        doublewidth xx width false
    | Right, (Char | Word), Reset, Chars, Equal ->
        [ (ecl, jointextlen) ] |> loopandprint
    | Right, (Char | Word), Reset, Columns, Equal -> ()
    | Right, Word, Reset, Chars, Greater -> begin
        let l, r =
          true |> brword
          |> List.split_while ~f:(fun (a, _) -> a > halflen + ecl)
        in
        takeappend r l |> loopandprint
      end
    | Right, Word, Reset, Columns, Greater -> ()
    | Right, Word, Wrap, Chars, (Greater | Equal | Less) ->
        List.take (brword true) (List.length text) |> loopandprint
    | Right, Word, Wrap, Columns, (Greater | Equal | Less) -> ()
  end
