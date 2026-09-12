open Core

type t = {
  cycles : int;
  endcap_char : char;
  rest : int;
  step : Sb.Step.t;
  sleep : int;
}

let run_bounce text Universal.{ prefix; suffix; terminator; width }
    { cycles; endcap_char; rest; step; sleep } =
  let joined_bytes, jointextlen, visual_chars = Sb.sbvals text in
  let ecl = Int.max 0 (width - visual_chars) in
  let ecp = Bytes.make ecl endcap_char in
  let finaltext = Stdlib.Bytes.cat (Stdlib.Bytes.cat ecp joined_bytes) ecp in
  let ltfunc pwlist _ =
    let tot = sleep + rest in
    let maxpos =
      let rec loop max = function
        | [] -> max
        | (p, _) :: t -> (loop [@tailcall]) (if p > max then p else max) t
      in
      loop 0 pwlist
    in
    List.map pwlist ~f:(fun (p, w) ->
        if p = 0 || p = maxpos then (p, w, tot) else (p, w, sleep))
  in
  let ( loopandprint,
        term,
        lenminuswidth,
        _,
        brword,
        blchar,
        blword,
        _,
        takeappend,
        totallen ) =
    Sb.sbfuncs ltfunc finaltext
      Universal.{ prefix; suffix; terminator; width }
      cycles
  in
  begin
    let open Sb.Step in
    match (step, Ordering.of_int (compare visual_chars width)) with
    | Char, (Greater | Equal | Less) ->
        let l, r =
          List.split_while blchar ~f:(fun (a, b) -> a + b < totallen)
        in
        List.drop l 1 |> List.rev
        |> List.append (takeappend r l)
        |> loopandprint
    | Word, Greater -> begin
        brword |> List.append blword
        |> List.filteri ~f:(fun i (a, _) ->
            (a > 0 || i = 0) && a <= lenminuswidth)
        |> List.remove_consecutive_duplicates ~which_to_keep:`Last
             ~equal:(fun (a, _) (b, _) -> a = b)
        |> loopandprint
      end
    | Word, (Equal | Less) ->
        [ (0, jointextlen + ecl); (lenminuswidth, jointextlen + ecl) ]
        |> loopandprint
  end;
  term
