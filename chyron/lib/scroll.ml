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
  let joined_bytes, jointextlen, visual_chars = Sb.sbvals text visual in
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
  let joined_bytes, jointextlen, visual_cols = Sb.sbvals text visual in
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
  let splitchar width sleep mc lt =
    (* print_endline @@ string_of_int @@ List.length lt; *)
    if width = 2 && mc > 1 then ("Q", 1, sleep) :: lt else lt
  in

  let splitword _ _ _ lt =
    List.drop
      (lt |> List.drop_while ~f:(fun (char, _, _) -> String.( <> ) char "H"))
      1
  in

  let prelims lt ind =
    let tot = sleep + rest in
    List.mapi lt ~f:(fun i char ->
        let width = Universal.eaw char in
        if i = 0 || i = ind then (char, width, tot) else (char, width, sleep))
  in

  let final_extends lt =
    let a, b, c, d, _ =
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
    (* print_endline @@ string_of_int b;
    print_endline @@ string_of_int d;
    print_endline @@ string_of_int e; *)
    if width - d = 2 then (a, c, pred b) else (a, c, b)
  in

  begin
    let open Direction in
    let open Sb.Step in
    let open Mode in
    match
      (direction, step, mode, Ordering.of_int (compare visual_cols width))
    with
    | Left, Char, Reset, Greater ->
        let charlist = Universal.uc_charlist (Bytes.to_string joined_bytes) in
        let prelim = prelims charlist (pred (List.length charlist)) in
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
    | Left, (Char | Word), Reset, Equal ->
        let charlist = Universal.uc_charlist (Bytes.to_string joined_bytes) in
        let prelim =
          prelims (List.rev charlist) (pred (List.length charlist))
        in
        let finallist, extends, mcount = final_extends prelim in
        Sb.doublewidth finallist extends Fn.id width cycles print term
          jointextlen splitchar mcount
    | Left, Word, Reset, Greater ->
        let charlist = Universal.uc_charlist (Bytes.to_string joined_bytes) in
        let prelim = prelims charlist (pred (List.length charlist)) in
        Sb.doublewidth (List.rev prelim) [] List.rev width cycles print term
          jointextlen splitword 0
    | Left, Word, Wrap, (Greater | Equal | Less) ->
        let charlist =
          Universal.uc_charlist
            (Bytes.to_string (Stdlib.Bytes.cat joined_bytes ecp))
        in
        let prelim = prelims charlist 0 in
        Sb.doublewidth (List.rev prelim) (List.rev prelim) List.rev width cycles
          print term jointextlen splitword 0
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
    | Right, Char, Reset, Greater ->
        let charlist = Universal.uc_charlist (Bytes.to_string joined_bytes) in
        (* print_endline @@ List.to_string ~f:Fn.id charlist; *)
        let prelim =
          prelims (List.rev charlist) (pred (List.length charlist))
        in
        (* Printf.printf "lt:  %s -- \n"
          (List.to_string
             ~f:(fun (a, b, c) ->
               a ^ "_" ^ string_of_int b ^ "_" ^ string_of_int c)
             prelim); *)
        let finallist, extends, mcount = final_extends prelim in

        (* Printf.printf "%s \n"
          (List.to_string
             ~f:(fun (a, b, c) ->
               a ^ "_" ^ string_of_int b ^ "_" ^ string_of_int c)
             finallist);
        Printf.printf "%s \n"
          (List.to_string
             ~f:(fun (a, b, c) ->
               a ^ "_" ^ string_of_int b ^ "_" ^ string_of_int c)
             extends); *)
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
    | Right, (Char | Word), Reset, Equal ->
        let charlist = Universal.uc_charlist (Bytes.to_string joined_bytes) in
        let prelim =
          prelims (List.rev charlist) (pred (List.length charlist))
        in
        let finallist, extends, mcount = final_extends prelim in
        Sb.doublewidth finallist extends Fn.id width cycles print term
          jointextlen splitchar mcount
    | Right, Word, Reset, Greater ->
        let charlist = Universal.uc_charlist (Bytes.to_string joined_bytes) in
        let prelim = prelims charlist (pred (List.length charlist)) in
        Sb.doublewidth prelim [] Fn.id width cycles print term jointextlen
          splitword 0
    | Right, Word, Wrap, (Greater | Equal | Less) ->
        let charlist =
          Universal.uc_charlist
            (Bytes.to_string (Stdlib.Bytes.cat ecp joined_bytes))
        in
        let prelim = prelims charlist 0 in
        Sb.doublewidth prelim prelim Fn.id width cycles print term jointextlen
          splitword 0
  end
