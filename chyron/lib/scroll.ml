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
  print_endline @@ string_of_int visual_cols;
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
  let fchar cw sl lt = if cw = 2 then (" ", 1, sl) :: lt else lt in
  let fword _ _ lt =
    List.drop
      (lt |> List.drop_while ~f:(fun (c, _, _) -> String.( <> ) c " "))
      1
  in
  begin
    let open Direction in
    let open Sb.Step in
    let open Mode in
    match
      (direction, step, mode, Ordering.of_int (compare visual_cols width))
    with
    | Left, Char, Reset, Greater ->
        print_endline "here 3";
        let charlist = Universal.uc_charlist (Bytes.to_string joined_bytes) in

        let prelim =
          let tot = sleep + rest in

          List.mapi charlist ~f:(fun i ch ->
              let wd = Universal.eaw ch in
              if i = 0 || i = pred (List.length charlist) then (ch, wd, tot)
              else (ch, wd, sleep))
        in

        let finallist, extends, _ =
          List.fold prelim ~init:([], [], 0)
            ~f:(fun (lt, ex, count) (char, wth, slp) ->
              let tot = wth + count in

              if tot >= width then ((char, wth, slp) :: lt, ex, count)
              else (lt, (char, wth, slp) :: ex, tot))
        in

        (* let extends, _ =
          List.fold prelim ~init:([], 0) ~f:(fun (lt, count) (char, wth, slp) ->
              if count >= width then (lt, count)
              else ((char, wth, slp) :: lt, wth + count))
        in *)
        Printf.printf "%s \n"
          (List.to_string
             ~f:(fun (a, b, c) ->
               a ^ "_" ^ string_of_int b ^ "_" ^ string_of_int c)
             finallist);
        Printf.printf "%s \n"
          (List.to_string
             ~f:(fun (a, b, c) ->
               a ^ "_" ^ string_of_int b ^ "_" ^ string_of_int c)
             extends);

        Sb.doublewidth finallist extends List.rev width cycles print term
          jointextlen fchar
    | Left, Char, Wrap, (Greater | Equal | Less) ->
        print_endline "here 2";
        let charlist =
          Universal.uc_charlist
            (Bytes.to_string (Stdlib.Bytes.cat joined_bytes ecp))
        in

        let finallist =
          let tot = sleep + rest in

          List.mapi (List.rev charlist) ~f:(fun i ch ->
              let wd = Universal.eaw ch in
              if i = 0 then (ch, wd, tot) else (ch, wd, sleep))
          (* | Reset ->
              List.mapi revcharlist ~f:(fun i ch ->
                  let wd = eaw ch in
                  if i = 0 || i = pred (List.length revcharlist) then
                    (ch, wd, tot)
                  else (ch, wd, sleep)) *)
        in

        let extends, _ =
          List.fold finallist ~init:([], 0)
            ~f:(fun (lt, count) (char, wth, slp) ->
              if count >= width then (lt, count)
              else ((char, wth, slp) :: lt, wth + count))
        in

        Printf.printf "%s \n"
          (List.to_string
             ~f:(fun (a, b, c) ->
               a ^ "_" ^ string_of_int b ^ "_" ^ string_of_int c)
             finallist);
        Printf.printf "%s \n"
          (List.to_string
             ~f:(fun (a, b, c) ->
               a ^ "_" ^ string_of_int b ^ "_" ^ string_of_int c)
             extends);

        Sb.doublewidth finallist (List.rev extends) List.rev width cycles print
          term jointextlen fchar
    | Left, (Char | Word), Reset, Equal ->
        print_endline "lc/wre";
        let charlist = Universal.uc_charlist (Bytes.to_string joined_bytes) in

        let prelim =
          let tot = sleep + rest in

          List.mapi (List.rev charlist) ~f:(fun i ch ->
              let wd = Universal.eaw ch in
              if i = 0 || i = pred (List.length charlist) then (ch, wd, tot)
              else (ch, wd, sleep))
        in

        let finallist, extends, _ =
          List.fold prelim ~init:([], [], 0)
            ~f:(fun (lt, ex, count) (char, wth, slp) ->
              let tot = wth + count in

              if tot >= width then ((char, wth, slp) :: lt, ex, count)
              else (lt, (char, wth, slp) :: ex, tot))
        in

        (* let extends, _ =
          List.fold prelim ~init:([], 0) ~f:(fun (lt, count) (char, wth, slp) ->
              if count >= width then (lt, count)
              else ((char, wth, slp) :: lt, wth + count))
        in *)
        Printf.printf "%s \n"
          (List.to_string
             ~f:(fun (a, b, c) ->
               a ^ "_" ^ string_of_int b ^ "_" ^ string_of_int c)
             finallist);
        Printf.printf "%s \n"
          (List.to_string
             ~f:(fun (a, b, c) ->
               a ^ "_" ^ string_of_int b ^ "_" ^ string_of_int c)
             extends);

        Sb.doublewidth finallist extends Fn.id width cycles print term
          jointextlen fchar
    | Left, Word, Reset, Greater ->
        print_endline "aa";
        let charlist = Universal.uc_charlist (Bytes.to_string joined_bytes) in

        let prelim =
          let tot = sleep + rest in

          List.mapi charlist ~f:(fun i ch ->
              let wd = Universal.eaw ch in
              if i = 0 || i = pred (List.length charlist) then (ch, wd, tot)
              else (ch, wd, sleep))
        in

        (* let finallist, extends, _ =
          List.fold prelim ~init:([], [], 0)
            ~f:(fun (lt, ex, count) (char, wth, slp) ->
              let tot = wth + count in

              if tot >= width then ((char, wth, slp) :: lt, ex, count)
              else (lt, (char, wth, slp) :: ex, tot))
        in *)

        (* let extends, _ =
          List.fold prelim ~init:([], 0) ~f:(fun (lt, count) (char, wth, slp) ->
              if count >= width then (lt, count)
              else ((char, wth, slp) :: lt, wth + count))
        in *)
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
        Sb.doublewidth (List.rev prelim) [] List.rev width cycles print term
          jointextlen fword
    | Left, Word, Wrap, (Greater | Equal | Less) ->
        print_endline "bb";

        let charlist =
          Universal.uc_charlist
            (Bytes.to_string (Stdlib.Bytes.cat joined_bytes ecp))
        in

        let prelim =
          let tot = sleep + rest in

          List.mapi charlist ~f:(fun i ch ->
              let wd = Universal.eaw ch in
              if i = 0 || i = pred (List.length charlist) then (ch, wd, tot)
              else (ch, wd, sleep))
        in

        (* let finallist, extends, _ =
          List.fold prelim ~init:([], [], 0)
            ~f:(fun (lt, ex, count) (char, wth, slp) ->
              let tot = wth + count in

              if tot >= width then ((char, wth, slp) :: lt, ex, count)
              else (lt, (char, wth, slp) :: ex, tot))
        in *)

        (* let extends, _ =
          List.fold prelim ~init:([], 0) ~f:(fun (lt, count) (char, wth, slp) ->
              if count >= width then (lt, count)
              else ((char, wth, slp) :: lt, wth + count))
        in *)
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
        Sb.doublewidth (List.rev prelim) (List.rev prelim) List.rev width cycles
          print term jointextlen fword
    | Left, (Char | Word), Reset, Less ->
        print_endline "l/rc/wrl";
        let charlist =
          Universal.uc_charlist
            (Bytes.to_string (Stdlib.Bytes.cat joined_bytes ecp))
        in

        let prelim =
          let tot = sleep + rest in

          List.mapi (List.rev charlist) ~f:(fun i ch ->
              let wd = Universal.eaw ch in
              if i = 0 || i = pred (List.length charlist) then (ch, wd, tot)
              else (ch, wd, sleep))
        in

        let finallist, extends, _ =
          List.fold prelim ~init:([], [], 0)
            ~f:(fun (lt, ex, count) (char, wth, slp) ->
              let tot = wth + count in

              if tot >= width then ((char, wth, slp) :: lt, ex, count)
              else (lt, (char, wth, slp) :: ex, tot))
        in

        (* let extends, _ =
          List.fold prelim ~init:([], 0) ~f:(fun (lt, count) (char, wth, slp) ->
              if count >= width then (lt, count)
              else ((char, wth, slp) :: lt, wth + count))
        in *)
        Printf.printf "%s \n"
          (List.to_string
             ~f:(fun (a, b, c) ->
               a ^ "_" ^ string_of_int b ^ "_" ^ string_of_int c)
             finallist);
        Printf.printf "%s \n"
          (List.to_string
             ~f:(fun (a, b, c) ->
               a ^ "_" ^ string_of_int b ^ "_" ^ string_of_int c)
             extends);

        Sb.doublewidth finallist extends Fn.id width cycles print term
          jointextlen fchar
    | Right, (Char | Word), Reset, Less ->
        print_endline "l/rc/wrl";
        let charlist =
          Universal.uc_charlist
            (Bytes.to_string (Stdlib.Bytes.cat ecp joined_bytes))
        in

        let prelim =
          let tot = sleep + rest in

          List.mapi (List.rev charlist) ~f:(fun i ch ->
              let wd = Universal.eaw ch in
              if i = 0 || i = pred (List.length charlist) then (ch, wd, tot)
              else (ch, wd, sleep))
        in

        let finallist, extends, _ =
          List.fold prelim ~init:([], [], 0)
            ~f:(fun (lt, ex, count) (char, wth, slp) ->
              let tot = wth + count in

              if tot >= width then ((char, wth, slp) :: lt, ex, count)
              else (lt, (char, wth, slp) :: ex, tot))
        in

        (* let extends, _ =
          List.fold prelim ~init:([], 0) ~f:(fun (lt, count) (char, wth, slp) ->
              if count >= width then (lt, count)
              else ((char, wth, slp) :: lt, wth + count))
        in *)
        Printf.printf "%s \n"
          (List.to_string
             ~f:(fun (a, b, c) ->
               a ^ "_" ^ string_of_int b ^ "_" ^ string_of_int c)
             finallist);
        Printf.printf "%s \n"
          (List.to_string
             ~f:(fun (a, b, c) ->
               a ^ "_" ^ string_of_int b ^ "_" ^ string_of_int c)
             extends);

        Sb.doublewidth finallist extends Fn.id width cycles print term
          jointextlen fchar
    | Right, Char, Reset, Greater ->
        print_endline "here 4";
        let charlist = Universal.uc_charlist (Bytes.to_string joined_bytes) in

        let prelim =
          let tot = sleep + rest in

          List.mapi (List.rev charlist) ~f:(fun i ch ->
              let wd = Universal.eaw ch in
              if i = 0 || i = pred (List.length charlist) then (ch, wd, tot)
              else (ch, wd, sleep))
        in

        let finallist, extends, _ =
          List.fold prelim ~init:([], [], 0)
            ~f:(fun (lt, ex, count) (char, wth, slp) ->
              let tot = wth + count in

              if tot >= width then ((char, wth, slp) :: lt, ex, count)
              else (lt, (char, wth, slp) :: ex, tot))
        in

        (* let extends, _ =
          List.fold prelim ~init:([], 0) ~f:(fun (lt, count) (char, wth, slp) ->
              if count >= width then (lt, count)
              else ((char, wth, slp) :: lt, wth + count))
        in *)
        Printf.printf "%s \n"
          (List.to_string
             ~f:(fun (a, b, c) ->
               a ^ "_" ^ string_of_int b ^ "_" ^ string_of_int c)
             finallist);
        Printf.printf "%s \n"
          (List.to_string
             ~f:(fun (a, b, c) ->
               a ^ "_" ^ string_of_int b ^ "_" ^ string_of_int c)
             extends);

        Sb.doublewidth finallist extends Fn.id width cycles print term
          jointextlen fchar
    | Right, Char, Wrap, (Greater | Equal | Less) ->
        print_endline "here 1";
        let charlist =
          Universal.uc_charlist
            (Bytes.to_string (Stdlib.Bytes.cat ecp joined_bytes))
        in

        let finallist =
          let tot = sleep + rest in
          (* let open Mode in *)
          (* match mode with
          | Wrap -> *)
          List.mapi charlist ~f:(fun i ch ->
              let wd = Universal.eaw ch in
              if i = 0 then (ch, wd, tot) else (ch, wd, sleep))
          (* | Reset ->
              List.mapi charlist ~f:(fun i ch ->
                  let wd = Universal.eaw ch in
                  if i = 0 || i = pred (List.length charlist) then (ch, wd, tot)
                  else (ch, wd, sleep)) *)
        in

        let extends, _ =
          List.fold finallist ~init:([], 0)
            ~f:(fun (lt, count) (char, wth, slp) ->
              if count >= width then (lt, count)
              else ((char, wth, slp) :: lt, wth + count))
        in

        Printf.printf "%s \n"
          (List.to_string
             ~f:(fun (a, b, c) ->
               a ^ "_" ^ string_of_int b ^ "_" ^ string_of_int c)
             finallist);
        Printf.printf "%s \n"
          (List.to_string
             ~f:(fun (a, b, c) ->
               a ^ "_" ^ string_of_int b ^ "_" ^ string_of_int c)
             extends);

        Sb.doublewidth finallist (List.rev extends) Fn.id width cycles print
          term jointextlen fchar
    | Right, (Char | Word), Reset, Equal ->
        print_endline "cc";
        let charlist = Universal.uc_charlist (Bytes.to_string joined_bytes) in

        let prelim =
          let tot = sleep + rest in

          List.mapi (List.rev charlist) ~f:(fun i ch ->
              let wd = Universal.eaw ch in
              if i = 0 || i = pred (List.length charlist) then (ch, wd, tot)
              else (ch, wd, sleep))
        in

        let finallist, extends, _ =
          List.fold prelim ~init:([], [], 0)
            ~f:(fun (lt, ex, count) (char, wth, slp) ->
              let tot = wth + count in

              if tot >= width then ((char, wth, slp) :: lt, ex, count)
              else (lt, (char, wth, slp) :: ex, tot))
        in

        (* let extends, _ =
          List.fold prelim ~init:([], 0) ~f:(fun (lt, count) (char, wth, slp) ->
              if count >= width then (lt, count)
              else ((char, wth, slp) :: lt, wth + count))
        in *)
        Printf.printf "%s \n"
          (List.to_string
             ~f:(fun (a, b, c) ->
               a ^ "_" ^ string_of_int b ^ "_" ^ string_of_int c)
             finallist);
        Printf.printf "%s \n"
          (List.to_string
             ~f:(fun (a, b, c) ->
               a ^ "_" ^ string_of_int b ^ "_" ^ string_of_int c)
             extends);

        Sb.doublewidth finallist extends Fn.id width cycles print term
          jointextlen fchar
    | Right, Word, Reset, Greater -> print_endline "dd"
    | Right, Word, Wrap, (Greater | Equal | Less) -> print_endline "ee"
  end
