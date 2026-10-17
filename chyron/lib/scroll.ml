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

let run_scroll_chars text
    Universal.{ prefix; suffix; terminator; visual; width }
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
  let ltfunc pwlist =
    let tot = sleep + rest in
    let open Mode in
    match mode with
    | Wrap ->
        List.mapi pwlist ~f:(fun i (p, w) ->
            if i = 0 then (p, w, tot) else (p, w, sleep))
    | Reset ->
        List.mapi pwlist ~f:(fun i (p, w) ->
            if i = 0 || i = pred (List.length pwlist) then (p, w, tot)
            else (p, w, sleep))
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
    let open Sb.Step in
    let open Mode in
    match
      (direction, step, mode, Ordering.of_int (compare visual_chars width))
    with
    | Left, Char, Reset, Greater ->
        true |> blchar
        |> List.filter ~f:(fun (a, b) -> a + b <= halflen - ecl)
        |> loopandprint
    | Left, Char, Wrap, (Greater | Equal | Less) ->
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

let run_scroll_cols text Universal.{ prefix; suffix; terminator; visual; width }
    { cycles; direction; endcap_char; endcap_len; rest; mode; step; sleep } =
  let joined_bytes, jointextlen, visual_chars = Sb.sbvals text in
  let ecl =
    Int.clamp_exn
      (Int.max endcap_len (width - visual_chars))
      ~min:1 ~max:(pred width)
  in
  let ecp = Bytes.make ecl endcap_char in
  let finaltext =
    let open Direction in
    match direction with
    | Left -> Stdlib.Bytes.cat joined_bytes ecp
    | Right -> Stdlib.Bytes.cat ecp joined_bytes
  in
  let charlist = Universal.uc_charlist (Bytes.to_string finaltext) in
  let revcharlist = List.rev charlist in
  let open Universal in
  let print, term =
    printandterm { prefix; suffix; terminator; visual; width }
  in
  begin
    let open Direction in
    let open Sb.Step in
    let open Mode in
    match
      (direction, step, mode, Ordering.of_int (compare visual_chars width))
    with
    | Left, Char, Reset, Greater -> ()
    | Left, Char, Wrap, (Greater | Equal | Less) ->
        print_endline "here 2";
        let xx =
          List.map revcharlist ~f:(fun x ->
              let a =
                String.Utf8.get (String.Utf8.of_string_unchecked x) ~byte_pos:0
              in
              let b = Uucp.Break.east_asian_width a in
              let c = match b with `W | `F -> 2 | _ -> 1 in
              (x, c))
        in
        let ff =
          List.fold xx ~init:(0, []) ~f:(fun acc x ->
              if fst acc >= width then acc else (snd x + fst acc, x :: snd acc))
        in
        let pp = List.append xx (List.rev (snd ff)) in

        let lt =
          let tot = sleep + rest in
          let open Mode in
          match mode with
          | Wrap ->
              List.mapi pp ~f:(fun i (c, w) ->
                  if i = 0 then (c, w, tot) else (c, w, sleep))
          | Reset ->
              List.mapi pp ~f:(fun i (c, w) ->
                  if i = 0 || i = pred (List.length pp) then (c, w, tot)
                  else (c, w, sleep))
        in

        Sb.doublewidth lt true width cycles print term jointextlen
    | Left, (Char | Word), Reset, Equal -> ()
    | Left, Word, Reset, Greater -> ()
    | Left, Word, Wrap, (Greater | Equal | Less) -> ()
    | (Left | Right), (Char | Word), Reset, Less -> ()
    | Right, Char, Reset, Greater -> ()
    | Right, Char, Wrap, (Greater | Equal | Less) ->
        print_endline "here 1";
        let xx =
          List.map charlist ~f:(fun x ->
              let a =
                String.Utf8.get (String.Utf8.of_string_unchecked x) ~byte_pos:0
              in
              let b = Uucp.Break.east_asian_width a in
              let c = match b with `W | `F -> 2 | _ -> 1 in
              (x, c))
        in
        let ff =
          List.fold xx ~init:(0, []) ~f:(fun acc x ->
              if fst acc >= width then acc else (snd x + fst acc, x :: snd acc))
        in

        let pp = List.append xx (List.rev (snd ff)) in
        let lt =
          let tot = sleep + rest in
          let open Mode in
          match mode with
          | Wrap ->
              List.mapi pp ~f:(fun i (c, w) ->
                  if i = 0 then (c, w, tot) else (c, w, sleep))
          | Reset ->
              List.mapi pp ~f:(fun i (c, w) ->
                  if i = 0 || i = pred (List.length pp) then (c, w, tot)
                  else (c, w, sleep))
        in

        (* Printf.printf "%s "
          (List.to_string ~f:(fun (a, b) -> a ^ "_" ^ string_of_int b) pp);
        print_endline "";
        Printf.printf "%s "
          (List.to_string
             ~f:(fun (a, b) -> a ^ "_" ^ string_of_int b)
             (List.append xx xx)); *)
        Sb.doublewidth lt false width cycles print term jointextlen
    | Right, (Char | Word), Reset, Equal -> ()
    | Right, Word, Reset, Greater -> ()
    | Right, Word, Wrap, (Greater | Equal | Less) -> ()
  end
