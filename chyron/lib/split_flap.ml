open Core

type justify = Center | Left | Right [@@deriving sexp]

type charset = All | Lowers | Numbers | Symbols1 | Symbols2 | Uppers
[@@deriving equal, sexp]

type t = {
  charsets : charset list;
  cycles : int;
  flip_hi_bound : int;
  flip_lo_bound : int;
  flip_sleep : int;
  justify : justify;
  sleep : int;
}

let justify_arg =
  Command.Arg_type.of_alist_exn ~accept_unique_prefixes:true
    ~case_sensitive:false ~list_values_in_help:false
    [ ("center", Center); ("left", Left); ("right", Right) ]

let charset_arg =
  Command.Arg_type.comma_separated ~allow_empty:true ~strip_whitespace:true
    (Command.Arg_type.create (function
      | "all" -> All
      | "lowers" -> Lowers
      | "uppers" -> Uppers
      | "numbers" -> Numbers
      | "symbols1" -> Symbols1
      | "symbols2" -> Symbols2
      | _ -> invalid_arg "invalid selection"))

let lowers =
  [
    "a";
    "b";
    "c";
    "d";
    "e";
    "f";
    "g";
    "h";
    "i";
    "j";
    "k";
    "l";
    "m";
    "n";
    "o";
    "p";
    "q";
    "r";
    "s";
    "t";
    "u";
    "v";
    "w";
    "x";
    "y";
    "z";
    " ";
  ]

let uppers =
  [
    "A";
    "B";
    "C";
    "D";
    "E";
    "F";
    "G";
    "H";
    "I";
    "J";
    "K";
    "L";
    "M";
    "N";
    "O";
    "P";
    "Q";
    "R";
    "S";
    "T";
    "U";
    "V";
    "W";
    "X";
    "Y";
    "Z";
    " ";
  ]

let numbers = [ "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; " " ]
let symbols1 = [ "!"; "@"; "#"; "$"; "%"; "^"; "&"; "*"; "("; ")"; " " ]

let symbols2 =
  [
    "`";
    "~";
    "-";
    "_";
    "=";
    "+";
    "[";
    "]";
    "{";
    "}";
    "\\";
    "|";
    ";";
    ":";
    "'";
    "\"";
    ",";
    "<";
    ".";
    ">";
    "/";
    "?";
    " ";
  ]

let run_split_flap text Universal.{ prefix; suffix; terminator; width }
    {
      charsets;
      cycles;
      flip_hi_bound;
      flip_lo_bound;
      flip_sleep;
      justify;
      sleep;
    } =
  let rec list_concat ~sep = function
    | [] -> []
    | [ s ] -> s
    | h :: t -> list_concat ~sep t |> List.append (sep |> List.append h)
  in
  let breakdown txt =
    let ltt =
      List.fold txt ~init:[] ~f:(fun acc x ->
          let lt = List.rev (Universal.uc_charlist x) in
          lt :: acc)
      |> List.rev
    in
    let rec loop txt =
      match List.for_all txt ~f:(fun x -> List.length x <= width) with
      | true -> txt
      | false ->
          List.fold (List.rev txt) ~init:[] ~f:(fun acc x ->
              let vis = List.length x in
              if vis > width then
                let l, r = List.split_n x (List.length x asr 1) in
                l :: r :: acc
              else x :: acc)
          |> (loop [@tailcall])
    in
    loop ltt
  in
  let buildup txt =
    let sub = List.sub txt in
    let rec loop acc pos len =
      let predlen = pred len in
      match pos + len > List.length txt with
      | true ->
          let lt = if predlen = 0 then acc else sub ~pos ~len:predlen :: acc in
          List.rev_map lt ~f:(fun x -> list_concat ~sep:[ " " ] x)
      | false -> begin
          let vis = list_concat ~sep:[ " " ] (sub ~pos ~len) |> List.length in
          match Ordering.of_int (compare vis width) with
          | Less -> (loop [@tailcall]) acc pos (succ len)
          | Greater ->
              (loop [@tailcall])
                (sub ~pos ~len:predlen :: acc)
                (pos + predlen) 1
          | Equal -> (loop [@tailcall]) (sub ~pos ~len :: acc) (pos + len) 1
        end
    in
    loop [] 0 1
  in
  let pad txt =
    List.map txt ~f:(fun x ->
        let diff = width - List.length x in
        let intspace _ = " " in
        match (justify, diff = 0) with
        | _, true -> x
        | Left, false -> list_concat ~sep:[] [ x; List.init diff ~f:intspace ]
        | Right, false -> list_concat ~sep:[] [ List.init diff ~f:intspace; x ]
        | Center, false ->
            let r = diff / 2 in
            list_concat ~sep:[]
              [ List.init r ~f:intspace; x; List.init (diff - r) ~f:intspace ])
  in

  let rec dedup_charsets = function
    | [] -> []
    | h :: t ->
        if List.mem t h ~equal:(fun x y -> equal_charset x y) then
          h
          :: dedup_charsets
               (List.filter t ~f:(fun x -> not (equal_charset h x)))
        else h :: dedup_charsets t
  in

  let letters =
    let rec loop acc = function
      | [] -> List.rev acc |> list_concat ~sep:[]
      | h :: t ->
          let lt =
            match h with
            | All -> lowers
            | Lowers -> lowers
            | Uppers -> uppers
            | Numbers -> numbers
            | Symbols1 -> symbols1
            | Symbols2 -> symbols2
          in
          loop (lt :: acc) t
    in
    loop []
      (if List.mem charsets All ~equal:(fun x y -> equal_charset x y) then
         [ Lowers; Uppers; Numbers; Symbols1; Symbols2 ]
       else dedup_charsets charsets)
  in

  print_endline @@ List.to_string ~f:Fn.id letters;

  let finaltex = text |> breakdown |> buildup |> pad in
  let letters_arr = Array.of_list letters in
  let len_letters = Array.length letters_arr in
  let input_concat = String.concat text in
  let len_input_concat = String.length input_concat in
  let excess_bytes =
    len_input_concat - List.length (Universal.uc_charlist input_concat)
  in
  let buffer = Bytes.create ((width lsl 2) + excess_bytes) in
  let print, term =
    Universal.printandterm Universal.{ prefix; suffix; terminator; width }
  in
  let rec run_workers counts ~letters ~flips =
    if flips <= 0 then ()
    else begin
      let line =
        List.map2_exn counts letters ~f:(fun c l ->
            if c < 1 then l
            else Random.int len_letters |> Array.unsafe_get letters_arr)
        |> String.concat ~sep:""
      in
      let llen = String.length line in
      Bytes.From_string.unsafe_blit ~src:line ~src_pos:0 ~dst:buffer ~dst_pos:0
        ~len:llen;
      print buffer 0 llen;
      Universal.Externs.caml_clock_nanosleep flip_sleep;
      (run_workers [@tailcall]) (List.map counts ~f:pred) ~letters
        ~flips:(pred flips)
    end
  in
  let loopandprint wordlist =
    let wl_len = List.length wordlist in
    let wordarray = Array.of_list wordlist in
    let rec loop ticks idx =
      if ticks <= 0 then term ()
      else begin
        List.init width ~f:(fun _ ->
            Random.int_incl flip_lo_bound flip_hi_bound)
        |> run_workers
             ~letters:(Array.unsafe_get wordarray idx)
             ~flips:(succ flip_hi_bound);
        Universal.Externs.caml_clock_nanosleep sleep;
        let nidx = if idx = pred wl_len then 0 else succ idx in
        (loop [@tailcall]) (pred ticks) nidx
      end
    in
    loop (wl_len * cycles) 0
  in
  ();
  loopandprint finaltex
