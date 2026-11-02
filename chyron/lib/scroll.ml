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
  let joined_bytes, jointextlen, _, visual_chars = Sb.sbvals text visual in
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
  let joined_bytes, jointextlen, gclist, visual_cols = Sb.sbvals text visual in
  let ecl =
    Int.clamp_exn
      (Int.max endcap_len (width - visual_cols))
      ~min:1 ~max:(pred width)
  in
  let ecp = Bytes.make ecl endcap_char in
  let open Universal in
  let print, term =
    printandterm { prefix; suffix; terminator; visual; width }
  in

  let splitchar _ cwidth csleep mc lt =
    (* print_endline @@ string_of_int @@ List.length lt; *)
    let finlt = if cwidth = 2 && mc > 1 then (" ", 1, csleep) :: lt else lt in
    (finlt, 1)
  in

  let splitword cchar cwidth csleep mc lt =
    let open Mode in
    let th = match mode with Reset -> width | Wrap -> 0 in
    if mc <= th then ([], 0)
    else
      let discards, finlt =
        (cchar, cwidth, csleep) :: lt
        |> List.split_while ~f:(fun (nchar, _, _) -> String.( <> ) nchar " ")
      in
      (* Printf.printf "sw: %d %s -- \n" mc
        (List.to_string
           ~f:(fun (a, b, c) ->
             a ^ "_" ^ string_of_int b ^ "_" ^ string_of_int c)
           discards);
      Printf.printf "sw22: %d %s -- \n" mc
        (List.to_string
           ~f:(fun (a, b, c) ->
             a ^ "_" ^ string_of_int b ^ "_" ^ string_of_int c)
           finlt); *)

      if List.for_all finlt ~f:(fun (c, _, _) -> String.( = ) c " ") then ([], 0)
      else
        ( List.drop finlt 1,
          succ (List.fold discards ~init:0 ~f:(fun acc (_, w, _) -> acc + w)) )
  in

  let prelims lt ind =
    let tot = sleep + rest in
    List.mapi lt ~f:(fun i char ->
        let wid = Universal.eaw char in
        if i = 0 || i = ind then (char, wid, tot) else (char, wid, sleep))
  in

  let final_extends lt =
    let final, finlen, exts, extlen, _ =
      List.fold lt ~init:([], 0, [], 0, 0)
        ~f:(fun
            (acc_fin, accfinwid, acc_ext, accextwid, acc_count)
            (nchar, nwidth, nsleep)
          ->
          let tot = nwidth + acc_count in
          if tot >= width then
            ( (nchar, nwidth, nsleep) :: acc_fin,
              accfinwid + nwidth,
              acc_ext,
              accextwid,
              tot )
          else
            ( acc_fin,
              accfinwid,
              (nchar, nwidth, nsleep) :: acc_ext,
              accextwid + nwidth,
              tot ))
    in
    if width - extlen = 2 then (final, exts, pred finlen)
    else (final, exts, finlen)
  in

  begin
    let open Direction in
    let open Sb.Step in
    let open Mode in
    match
      (direction, step, mode, Ordering.of_int (compare visual_cols width))
    with
    | Left, Char, Reset, Greater ->
        let prelim = prelims gclist (pred (List.length gclist)) in
        let finallist, extends, mcount = final_extends prelim in
        Sb.doublewidth finallist extends List.rev width cycles print term
          jointextlen splitchar mcount
    | Left, Char, Wrap, (Greater | Equal | Less) ->
        let charlist =
          Universal.uc_charlist
            (Bytes.to_string (Stdlib.Bytes.cat joined_bytes ecp))
        in
        let finallist = prelims (List.rev charlist) 0 in
        let fincount =
          List.fold finallist ~init:0 ~f:(fun acc (_, w, _) -> acc + w)
        in
        let extends, excount =
          List.fold finallist ~init:([], 0)
            ~f:(fun (lt, count) (char, wth, slp) ->
              if count >= width then (lt, count)
              else ((char, wth, slp) :: lt, wth + count))
        in
        let adjcount =
          if width - excount = 2 then pred fincount else fincount
        in
        Sb.doublewidth finallist (List.rev extends) List.rev width cycles print
          term jointextlen splitchar adjcount
    | Left, Word, Reset, Greater ->
        let prelim = prelims gclist (pred (List.length gclist)) in
        let fincount =
          List.fold prelim ~init:0 ~f:(fun acc (_, w, _) -> acc + w)
        in
        Sb.doublewidth (List.rev prelim) [] List.rev width cycles print term
          jointextlen splitword fincount
    | Left, Word, Wrap, (Greater | Equal | Less) ->
        let charlist =
          Universal.uc_charlist
            (Bytes.to_string (Stdlib.Bytes.cat joined_bytes ecp))
        in
        let prelim = prelims charlist 0 in
        let fincount =
          List.fold prelim ~init:0 ~f:(fun acc (_, w, _) -> acc + w)
        in
        Sb.doublewidth (List.rev prelim) (List.rev prelim) List.rev width cycles
          print term jointextlen splitword fincount
    | Left, (Char | Word), Reset, Less ->
        let charlist =
          Universal.uc_charlist
            (Bytes.to_string (Stdlib.Bytes.cat joined_bytes ecp))
        in
        let prelim =
          prelims (List.rev charlist) (pred (List.length charlist))
        in
        let finallist, extends, mcount = final_extends prelim in
        Sb.doublewidth finallist extends Fn.id width cycles print term
          jointextlen splitchar mcount
    | Right, (Char | Word), Reset, Less ->
        let charlist =
          Universal.uc_charlist
            (Bytes.to_string (Stdlib.Bytes.cat ecp joined_bytes))
        in
        let prelim =
          prelims (List.rev charlist) (pred (List.length charlist))
        in
        let finallist, extends, mcount = final_extends prelim in
        Sb.doublewidth finallist extends Fn.id width cycles print term
          jointextlen splitchar mcount
    | Right, Char, Reset, Greater | (Left | Right), (Char | Word), Reset, Equal
      ->
        let prelim = prelims (List.rev gclist) (pred (List.length gclist)) in
        let finallist, extends, mcount = final_extends prelim in
        Sb.doublewidth finallist extends Fn.id width cycles print term
          jointextlen splitchar mcount
    | Right, Char, Wrap, (Greater | Equal | Less) ->
        let charlist =
          Universal.uc_charlist
            (Bytes.to_string (Stdlib.Bytes.cat ecp joined_bytes))
        in
        let finallist = prelims charlist 0 in
        let fincount =
          List.fold finallist ~init:0 ~f:(fun acc (_, w, _) -> acc + w)
        in
        let extends, excount =
          List.fold finallist ~init:([], 0)
            ~f:(fun (lt, count) (char, wth, slp) ->
              if count >= width then (lt, count)
              else ((char, wth, slp) :: lt, wth + count))
        in
        let adjcount =
          if width - excount = 2 then pred fincount else fincount
        in
        Sb.doublewidth finallist (List.rev extends) Fn.id width cycles print
          term jointextlen splitchar adjcount
    | Right, Word, Reset, Greater ->
        let prelim = prelims gclist (pred (List.length gclist)) in
        let fincount =
          List.fold prelim ~init:0 ~f:(fun acc (_, w, _) -> acc + w)
        in
        Sb.doublewidth prelim [] Fn.id width cycles print term jointextlen
          splitword fincount
    | Right, Word, Wrap, (Greater | Equal | Less) ->
        let charlist =
          Universal.uc_charlist
            (Bytes.to_string (Stdlib.Bytes.cat ecp joined_bytes))
        in
        let prelim = prelims charlist 0 in
        let fincount =
          List.fold prelim ~init:0 ~f:(fun acc (_, w, _) -> acc + w)
        in
        Sb.doublewidth prelim prelim Fn.id width cycles print term jointextlen
          splitword fincount
  end
