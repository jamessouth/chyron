open Core

module Scroll_mode = struct
  type t = Reset | Wrap [@@deriving sexp]

  let arg =
    Command.Arg_type.of_alist_exn ~accept_unique_prefixes:true
      ~case_sensitive:false ~list_values_in_help:false
      [ ("reset", Reset); ("wrap", Wrap) ]
end

module Ints = struct
  let parseint ~min num =
    match num |> int_of_string_opt with
    | Some n -> Int.max min n
    | None -> invalid_arg "not an int"

  let zeroplus = Command.Arg_type.create (parseint ~min:0)
  let oneplus = Command.Arg_type.create (parseint ~min:1)
  let twoplus = Command.Arg_type.create (parseint ~min:2)
end

module Scroll_step = struct
  type t = Char | Word [@@deriving sexp]

  let arg =
    Command.Arg_type.of_alist_exn ~accept_unique_prefixes:true
      ~case_sensitive:false ~list_values_in_help:false
      [ ("char", Char); ("word", Word) ]
end

module Terminator = struct
  type t = Newline | Return | Space [@@deriving sexp]

  let arg =
    Command.Arg_type.of_alist_exn ~accept_unique_prefixes:true
      ~case_sensitive:false ~list_values_in_help:false
      [ ("newline", Newline); ("return", Return); ("space", Space) ]
end

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

module Direction = struct
  type t = Left | Right [@@deriving sexp]

  let arg =
    Command.Arg_type.of_alist_exn ~accept_unique_prefixes:true
      ~case_sensitive:false ~list_values_in_help:false
      [ ("left", Left); ("right", Right) ]
end

module Justify = struct
  type t = Center | Left | Right [@@deriving sexp]

  let arg =
    Command.Arg_type.of_alist_exn ~accept_unique_prefixes:true
      ~case_sensitive:false ~list_values_in_help:false
      [ ("center", Center); ("left", Left); ("right", Right) ]
end

type universalflags = {
  prefix : string;
  suffix : string;
  terminator : Terminator.t;
  width : int;
}

type bounceflags = {
  endcap_char : char;
  endcap_len : int;
  rest : int;
  scroll_step : Scroll_step.t;
}

type scrollflags = { direction : Direction.t; scroll_mode : Scroll_mode.t }
type scrollbounceflags = { cycles : int; sleep : int }

type splitflapflags = {
  sfcycles : int;
  flip_hi_bound : int;
  flip_lo_bound : int;
  flip_sleep : int;
  justify : Justify.t;
  sfsleep : int;
}

let univfunc { prefix; suffix; terminator; _ } =
  (* setting these chars here instead of as constructor payloads because of the way the help text looks*)
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
    | _ ->
        Externs.unsafe_output_char stdout '\n';
        Externs.unsafe_flush stdout
  in
  (print, term)

let uc_charlist str =
  Uuseg_string.fold_utf_8 `Grapheme_cluster (fun acc char -> char :: acc) [] str

let run_split_flap text { prefix; suffix; terminator; width }
    { sfcycles; flip_hi_bound; flip_lo_bound; flip_sleep; justify; sfsleep } =
  let rec list_concat ~sep = function
    | [] -> []
    | [ s ] -> s
    | h :: t -> list_concat ~sep t |> List.append (sep |> List.append h)
  in
  let breakdown txt =
    let ltt =
      List.fold txt ~init:[] ~f:(fun acc x ->
          let lt = List.rev (uc_charlist x) in
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
  let finaltex = text |> breakdown |> buildup |> pad in
  let ltrs =
    Array.of_list
      [ "a"; "v"; "h"; "w"; "t"; "u"; "z"; "A"; "E"; "T"; "C"; "P"; "2"; "6" ]
  in
  let len_ltrs = Array.length ltrs in
  let input_concat = String.concat text in
  let len_input_concat = String.length input_concat in
  let excess_bytes =
    len_input_concat - List.length (uc_charlist input_concat)
  in
  let buffer = Bytes.create ((width lsl 2) + excess_bytes) in
  let print, term = univfunc { prefix; suffix; terminator; width } in
  let rec run_workers counts ~letters ~flips =
    if flips <= 0 then ()
    else begin
      let line =
        List.map2_exn counts letters ~f:(fun c l ->
            if c < 1 then l else Random.int len_ltrs |> Array.unsafe_get ltrs)
        |> String.concat ~sep:""
      in
      let llen = String.length line in
      Bytes.From_string.unsafe_blit ~src:line ~src_pos:0 ~dst:buffer ~dst_pos:0
        ~len:llen;
      print buffer 0 llen;
      Externs.caml_clock_nanosleep flip_sleep;
      (run_workers [@tailcall]) (List.map counts ~f:pred) ~letters
        ~flips:(pred flips)
    end
  in
  let loopandprint wordlist =
    let wl_len = List.length wordlist in
    let wordarray = Array.of_list wordlist in
    let rec loop ticks idx =
      if ticks <= 0 then ()
      else begin
        List.init width ~f:(fun _ ->
            Random.int_incl flip_lo_bound flip_hi_bound)
        |> run_workers
             ~letters:(Array.unsafe_get wordarray idx)
             ~flips:(succ flip_hi_bound);
        Externs.caml_clock_nanosleep sfsleep;
        let nidx = if idx = pred wl_len then 0 else succ idx in
        (loop [@tailcall]) (pred ticks) nidx
      end
    in
    loop (wl_len * sfcycles) 0
  in
  ();
  loopandprint finaltex;
  term

let sbvals text =
  let joined_text = String.concat ~sep:" " text in
  ( Bytes.of_string joined_text,
    String.length joined_text,
    List.length (uc_charlist joined_text) )

let sbfuncs ltfunc ft { prefix; suffix; terminator; width } cycles =
  let bytesofutfchars str visualchars =
    let bytelen, _ =
      Uuseg_string.fold_utf_8 `Grapheme_cluster
        (fun (bytecount, charcount) char ->
          if charcount >= visualchars then (bytecount, charcount)
          else (bytecount + String.length char, succ charcount))
        (0, 0) str
    in
    bytelen
  in
  let ucinds rev cl sptfn accfn wid =
    let rec loop pos acc = function
      | [] -> if rev then List.rev acc else acc
      | h :: t ->
          let lt = h :: t in
          let str = String.concat lt in
          let bts = bytesofutfchars str wid in
          let l, r = List.split_n lt (sptfn lt) in
          (loop [@tailcall])
            (String.length (String.concat l) + pos)
            (accfn str bts pos acc) r
    in
    loop 0 [] cl
  in
  let charlist = uc_charlist (Bytes.to_string ft) in
  let revcharlist = List.rev charlist in
  let totallen = Bytes.length ft in
  let wordsplitfn chr =
    chr
    |> List.take_while ~f:(fun s -> String.( <> ) s " ")
    |> List.length |> succ
  in
  let accmfn _ bts pos acc = (pos, bts) :: acc in
  let takeappend r l = List.take r 1 |> List.append l in
  let print, term = univfunc { prefix; suffix; terminator; width } in
  let loopandprint pwlist =
    let pwlen = List.length pwlist in
    let pwslist = ltfunc pwlist pwlen in
    let flatlist =
      let rec loop = function
        | [] -> []
        | (p, w, s) :: t -> p :: w :: s :: loop t
      in
      loop pwslist
    in
    let indexes = Array.of_list flatlist in
    let jumpdist = 3 in
    let arrlen = Array.length indexes - jumpdist in
    let rec loop ticks idx =
      if ticks <= 0 then ()
      else begin
        print ft
          (Array.unsafe_get indexes idx)
          (Array.unsafe_get indexes (succ idx));
        Externs.caml_clock_nanosleep
          (Array.unsafe_get indexes (idx |> succ |> succ));
        let nidx = if idx = arrlen then 0 else idx + jumpdist in
        (loop [@tailcall]) (pred ticks) nidx
      end
    in
    loop (pwlen * cycles) 0
  in
  ( loopandprint,
    term,
    totallen - bytesofutfchars (String.concat charlist) width,
    totallen asr 1,
    ucinds true charlist wordsplitfn
      (fun str bts _ acc -> (String.length str - bts, bts) :: acc)
      width,
    ucinds true revcharlist (fun _ -> 1) accmfn width,
    ucinds true revcharlist wordsplitfn accmfn width,
    ucinds false revcharlist (fun _ -> 1) accmfn width,
    takeappend,
    totallen )

let run_scroll text { prefix; suffix; terminator; width } { cycles; sleep }
    { direction; scroll_mode } { endcap_char; endcap_len; rest; scroll_step } =
  let joined_bytes, jointextlen, visual_chars = sbvals text in
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
    match scroll_mode with
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
    sbfuncs ltfunc finaltext { prefix; suffix; terminator; width } cycles
  in
  begin match
    ( direction,
      scroll_step,
      scroll_mode,
      Ordering.of_int (compare visual_chars width) )
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
  | Right, (Char | Word), Reset, Equal -> [ (ecl, jointextlen) ] |> loopandprint
  | Right, Word, Reset, Greater -> begin
      let l, r = List.split_while brword ~f:(fun (a, _) -> a > halflen + ecl) in
      takeappend r l |> loopandprint
    end
  | Right, Word, Wrap, (Greater | Equal | Less) ->
      List.take brword (List.length text) |> loopandprint
  end;
  term

let run_bounce text { prefix; suffix; terminator; width } { cycles; sleep }
    { endcap_char; endcap_len; rest; scroll_step } =
  let joined_bytes, jointextlen, visual_chars = sbvals text in
  let ecl = Int.max 0 (width - visual_chars) in
  let ecp = Bytes.make ecl endcap_char in
  let finaltext = Stdlib.Bytes.cat (Stdlib.Bytes.cat ecp joined_bytes) ecp in
  let ltfunc pwlist pwlen =
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
    sbfuncs ltfunc finaltext { prefix; suffix; terminator; width } cycles
  in
  begin match (scroll_step, Ordering.of_int (compare visual_chars width)) with
  | Char, (Greater | Equal | Less) ->
      let l, r = List.split_while blchar ~f:(fun (a, b) -> a + b < totallen) in
      List.drop l 1 |> List.rev |> List.append (takeappend r l) |> loopandprint
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
