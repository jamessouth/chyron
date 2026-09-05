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

module Scroll_unit = struct
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
  cycles : int;
  prefix : string;
  sleep : int;
  suffix : string;
  terminator : Terminator.t;
  width : int;
}

type bounceflags = {
  endcap_char : char;
  endcap_len : int;
  rest : int;
  scroll_unit : Scroll_unit.t;
}

type scrollflags = { direction : Direction.t; scroll_mode : Scroll_mode.t }

type splitflapflags = {
  flip_hi_bound : int;
  flip_lo_bound : int;
  flip_sleep : int;
  justify : Justify.t;
}

type worker_status = Active | Terminal
type worker = { letter : string; count : int; status : worker_status }

let run_split_flap text { cycles; prefix; sleep; suffix; terminator; width }
    { justify; flip_sleep; flip_hi_bound; flip_lo_bound } =
  let vclen_charlist str =
    let tl, cl =
      Uuseg_string.fold_utf_8 `Grapheme_cluster
        (fun (tlacc, clacc) char -> (succ tlacc, char :: clacc))
        (0, []) str
    in
    (tl, List.rev cl)
  in

  let rec list_concat ~sep = function
    | [] -> []
    | [ s ] -> s
    | h :: t -> List.append (List.append h sep) (list_concat ~sep t)
  in

  let breakdown txt =
    List.rev
      (List.fold txt ~init:[] ~f:(fun acc x ->
           let vis, lt = vclen_charlist x in
           if vis > width then
             let l, r = List.split_n lt (List.length lt asr 1) in
             r :: l :: acc
           else lt :: acc))
  in

  let buildup txt =
    let sub = List.sub txt in
    let rec loop acc pos len =
      Printf.printf "%d %d\n" pos len;
      let predlen = pred len in
      match pos + len > List.length txt with
      | true ->
          let lt = if predlen = 0 then acc else sub ~pos ~len:predlen :: acc in
          List.rev_map lt ~f:(fun x -> list_concat ~sep:[ " " ] x)
      | false -> begin
          let vis = List.length (list_concat ~sep:[ " " ] (sub ~pos ~len)) in
          match Ordering.of_int (compare vis width) with
          | Less -> loop acc pos (succ len)
          | Greater -> loop (sub ~pos ~len:predlen :: acc) (pos + predlen) 1
          | Equal -> loop (sub ~pos ~len :: acc) (pos + len) 1
        end
    in
    loop [] 0 1
  in

  let pad txt =
    List.map txt ~f:(fun x ->
        let diff = width - List.length x in
        Printf.printf " %d %s %s|\n" diff
          (String.t_of_sexp (Justify.sexp_of_t justify))
          (List.to_string ~f:Fn.id x);
        match (justify, diff = 0) with
        | _, true -> x
        | Left, false ->
            list_concat ~sep:[] [ x; List.init diff ~f:(fun _ -> " ") ]
        | Right, false ->
            list_concat ~sep:[] [ List.init diff ~f:(fun _ -> " "); x ]
        | Center, false ->
            let r = diff / 2 in
            list_concat ~sep:[]
              [
                List.init r ~f:(fun _ -> " ");
                x;
                List.init (diff - r) ~f:(fun _ -> " ");
              ])
  in

  let finaltex = pad (buildup (breakdown text)) in
  print_endline
    (List.to_string ~f:(fun j -> List.to_string ~f:Fn.id j) finaltex);

  let ltrs =
    Array.of_list
      [ "a"; "v"; "h"; "w"; "t"; "u"; "z"; "A"; "E"; "T"; "C"; "P"; "2"; "6" ]
  in
  let lenn = Array.length ltrs in
  let worker_step w =
    match w.status with
    | Terminal -> Some (w.letter, w)
    | Active ->
        if w.count <= 0 then Some (w.letter, { w with status = Terminal })
        else Some (ltrs.(Random.int lenn), { w with count = w.count - 1 })
  in

  let rec run_infinite_workers workers iterations_left =
    (* List.iter workers ~f:(fun x -> Printf.printf "%d" x.count);
    print_endline ""; *)
    if iterations_left <= 0 then ()
    else
      let stepped = List.map workers ~f:worker_step in

      let outputs =
        List.map stepped ~f:(function Some (v, _) -> v | None -> assert false)
      in
      let next_workers =
        List.map stepped ~f:(function
          | Some (_, next_w) -> next_w
          | None -> assert false)
      in

      let line = String.concat ~sep:"" outputs in
      Printf.printf "%s\r%!" line;
      Externs.caml_clock_nanosleep flip_sleep;
      run_infinite_workers next_workers (iterations_left - 1)
  in

  let loopandprint pwlist =
    let pwlen = List.length pwlist in
    let lines = Array.of_list pwlist in

    let rec loop ticks idx =
      if ticks <= 0 then ()
      else begin
        let generators =
          List.map
            ~f:(fun x ->
              let count = Random.int_incl flip_lo_bound flip_hi_bound in
              { letter = x; count; status = Active })
            (Array.unsafe_get lines idx)
        in
        run_infinite_workers generators (succ flip_hi_bound);
        Externs.caml_clock_nanosleep sleep;
        let nidx = if idx = pred pwlen then 0 else succ idx in
        (loop [@tailcall]) (pred ticks) nidx
      end
    in
    loop (pwlen * cycles) 0
  in
  ();

  loopandprint finaltex;

  (* let joined_text = String.concat ~sep:" " text in
    let _jointextlen = String.length joined_text in
  let visual_chars, _ = vclen_charlist joined_text in
  let _width_minus_visual_chars = width - visual_chars in
  let joined_bytes = Bytes.of_string joined_text in *)
  (* let lastchar =
    match terminator with Newline -> '\n' | Return -> '\r' | Space -> ' '
  in
  let pfix = Bytes.of_string prefix in
  let plen = Bytes.length pfix in
  let sfix = Bytes.of_string suffix in
  let slen = Bytes.length sfix in *)
  (* let print finalt =
    (* let finaltext = Bytes.of_string finalt in *)
    (* print_endline (string_of_int pos);
    print_endline (string_of_int wid); *)
    Externs.unsafe_output_bytes stdout pfix 0 plen;
    Externs.unsafe_output_bytes stdout finalt 0 width;
    Externs.unsafe_output_bytes stdout sfix 0 slen;
    Externs.unsafe_output_char stdout lastchar;
    Externs.unsafe_flush stdout
  in *)

  (* let bytesofutfchars str visualchars =
    let bytelen, _ =
      Uuseg_string.fold_utf_8 `Grapheme_cluster
        (fun (bytecount, charcount) char ->
          if charcount >= visualchars then (bytecount, charcount)
          else (bytecount + String.length char, succ charcount))
        (0, 0) str
    in
    bytelen
  in
  let _, charlist = vclen_charlist (Bytes.to_string finaltext) in
  let revcharlist = List.rev charlist in
  let totallen = Bytes.length finaltext in
  let _lenminuswidth =
    totallen - bytesofutfchars (String.concat charlist) width
  in
  let _halflen = totallen asr 1 in
  let ucinds rev cl sptfn accfn =
    let rec loop pos acc = function
      | [] -> if rev then List.rev acc else acc
      | h :: t ->
          let lt = h :: t in
          let str = String.concat lt in
          let bts = bytesofutfchars str width in
          let l, r = List.split_n lt (sptfn lt) in
          loop (String.length (String.concat l) + pos) (accfn str bts pos acc) r
    in
    loop 0 [] cl
  in
  let wordsplitfn chr =
    succ (List.length (List.take_while chr ~f:(fun s -> String.( <> ) s " ")))
  in
  let accmfn _ bts pos acc = (pos, bts) :: acc in
  let _brword =
    ucinds true charlist wordsplitfn (fun str bts _ acc ->
        (String.length str - bts, bts) :: acc)
  in
  let _blchar = ucinds true revcharlist (fun _ -> 1) accmfn in
  let _blword = ucinds true revcharlist wordsplitfn accmfn in
  let _rchar = ucinds false revcharlist (fun _ -> 1) accmfn in
  let _takeappend r l = List.append l (List.take r 1) in
  (); *)
  match terminator with
  | Newline -> ()
  | _ ->
      Externs.unsafe_output_char stdout '\n';
      Externs.unsafe_flush stdout

let run_scroll text { cycles; prefix; sleep; suffix; terminator; width }
    { direction; scroll_mode } { endcap_char; endcap_len; rest; scroll_unit } =
  let vclen_charlist str =
    let tl, cl =
      Uuseg_string.fold_utf_8 `Grapheme_cluster
        (fun (tlacc, clacc) char -> (succ tlacc, char :: clacc))
        (0, []) str
    in
    (tl, cl)
  in
  let joined_text = String.concat ~sep:" " text in
  let jointextlen = String.length joined_text in
  let visual_chars, _ = vclen_charlist joined_text in
  let width_minus_visual_chars = width - visual_chars in
  let ecl =
    Int.clamp_exn
      (Int.max endcap_len width_minus_visual_chars)
      ~min:1 ~max:(pred width)
  in
  let ecp = Bytes.make ecl endcap_char in
  let joined_bytes = Bytes.of_string joined_text in
  let base = Stdlib.Bytes.cat (Stdlib.Bytes.cat ecp joined_bytes) ecp in
  let finaltext =
    match direction with
    | Left -> Stdlib.Bytes.cat joined_bytes base
    | Right -> Stdlib.Bytes.cat base joined_bytes
  in
  let lastchar =
    match terminator with Newline -> '\n' | Return -> '\r' | Space -> ' '
  in
  let pfix = Bytes.of_string prefix in
  let plen = Bytes.length pfix in
  let sfix = Bytes.of_string suffix in
  let slen = Bytes.length sfix in
  let print pos wid =
    (* print_endline (string_of_int pos);
    print_endline (string_of_int wid); *)
    Externs.unsafe_output_bytes stdout pfix 0 plen;
    Externs.unsafe_output_bytes stdout finaltext pos wid;
    Externs.unsafe_output_bytes stdout sfix 0 slen;
    Externs.unsafe_output_char stdout lastchar;
    Externs.unsafe_flush stdout
  in
  let loopandprint pwlist =
    let pwlen = List.length pwlist in
    let tot = sleep + rest in
    let pwslist =
      match scroll_mode with
      | Wrap ->
          List.mapi pwlist ~f:(fun i (p, w) ->
              if i = 0 then (p, w, tot) else (p, w, sleep))
      | Reset ->
          List.mapi pwlist ~f:(fun i (p, w) ->
              if i = 0 || i = pred pwlen then (p, w, tot) else (p, w, sleep))
    in
    let flatlist =
      let rec loop = function
        | [] -> []
        | (p, w, s) :: t -> p :: w :: s :: loop t
      in
      loop pwslist
    in
    print_endline (List.to_string ~f:string_of_int flatlist);
    print_endline (Bytes.to_string finaltext);
    let indexes = Array.of_list flatlist in
    let arrlen = Array.length indexes - 3 in
    let rec loop ticks idx =
      if ticks <= 0 then ()
      else begin
        print
          (Array.unsafe_get indexes idx)
          (Array.unsafe_get indexes (succ idx));
        Externs.caml_clock_nanosleep (Array.unsafe_get indexes (idx + 2));
        let nidx = if idx = arrlen then 0 else idx + 3 in
        (loop [@tailcall]) (pred ticks) nidx
      end
    in
    loop (pwlen * cycles) 0
  in
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
  let _, charlist = vclen_charlist (Bytes.to_string finaltext) in
  let revcharlist = List.rev charlist in
  let totallen = Bytes.length finaltext in
  let lenminuswidth =
    totallen - bytesofutfchars (String.concat charlist) width
  in
  let halflen = totallen asr 1 in
  let ucinds rev cl sptfn accfn =
    let rec loop pos acc = function
      | [] -> if rev then List.rev acc else acc
      | h :: t ->
          let lt = h :: t in
          let str = String.concat lt in
          let bts = bytesofutfchars str width in
          let l, r = List.split_n lt (sptfn lt) in
          loop (String.length (String.concat l) + pos) (accfn str bts pos acc) r
    in
    loop 0 [] cl
  in
  let wordsplitfn chr =
    succ (List.length (List.take_while chr ~f:(fun s -> String.( <> ) s " ")))
  in
  let accmfn _ bts pos acc = (pos, bts) :: acc in
  let brword =
    ucinds true charlist wordsplitfn (fun str bts _ acc ->
        (String.length str - bts, bts) :: acc)
  in
  let blchar = ucinds true revcharlist (fun _ -> 1) accmfn in
  let blword = ucinds true revcharlist wordsplitfn accmfn in
  let rchar = ucinds false revcharlist (fun _ -> 1) accmfn in
  let takeappend r l = List.append l (List.take r 1) in
  begin match
    ( direction,
      scroll_unit,
      scroll_mode,
      Ordering.of_int (compare visual_chars width) )
  with
  | Left, Char, Reset, Greater ->
      List.filter blchar ~f:(fun (a, b) -> a + b <= halflen - ecl)
      |> loopandprint
  | Left, Char, Wrap, (Greater | Equal | Less) ->
      List.filter blchar ~f:(fun (a, _) -> a < halflen) |> loopandprint
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
      List.filter rchar ~f:(fun (a, _) ->
          a >= halflen + ecl && a < succ lenminuswidth)
      |> loopandprint
  | Right, Char, Wrap, (Greater | Equal | Less) ->
      List.filter rchar ~f:(fun (a, _) ->
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
  match terminator with
  | Newline -> ()
  | _ ->
      Externs.unsafe_output_char stdout '\n';
      Externs.unsafe_flush stdout

let run_bounce text { cycles; prefix; sleep; suffix; terminator; width }
    { endcap_char; endcap_len; rest; scroll_unit } =
  let vclen_charlist str =
    let tl, cl =
      Uuseg_string.fold_utf_8 `Grapheme_cluster
        (fun (tlacc, clacc) char -> (succ tlacc, char :: clacc))
        (0, []) str
    in
    (tl, cl)
  in
  let joined_text = String.concat ~sep:" " text in
  let jointextlen = String.length joined_text in
  let visual_chars, _ = vclen_charlist joined_text in
  let width_minus_visual_chars = width - visual_chars in
  let ecl = Int.max 0 width_minus_visual_chars in
  let ecp = Bytes.make ecl endcap_char in
  let joined_bytes = Bytes.of_string joined_text in
  let finaltext = Stdlib.Bytes.cat (Stdlib.Bytes.cat ecp joined_bytes) ecp in
  let lastchar =
    match terminator with Newline -> '\n' | Return -> '\r' | Space -> ' '
  in
  let pfix = Bytes.of_string prefix in
  let plen = Bytes.length pfix in
  let sfix = Bytes.of_string suffix in
  let slen = Bytes.length sfix in
  let print pos wid =
    (* print_endline (string_of_int pos);
    print_endline (string_of_int wid); *)
    Externs.unsafe_output_bytes stdout pfix 0 plen;
    Externs.unsafe_output_bytes stdout finaltext pos wid;
    Externs.unsafe_output_bytes stdout sfix 0 slen;
    Externs.unsafe_output_char stdout lastchar;
    Externs.unsafe_flush stdout
  in
  let loopandprint pwlist =
    let pwlen = List.length pwlist in
    let tot = sleep + rest in
    let pwslist =
      let maxpos =
        let rec loop max = function
          | [] -> max
          | (p, _) :: t -> loop (if p > max then p else max) t
        in
        loop 0 pwlist
      in
      List.map pwlist ~f:(fun (p, w) ->
          if p = 0 || p = maxpos then (p, w, tot) else (p, w, sleep))
    in
    let flatlist =
      let rec loop = function
        | [] -> []
        | (p, w, s) :: t -> p :: w :: s :: loop t
      in
      loop pwslist
    in
    print_endline (List.to_string ~f:string_of_int flatlist);
    print_endline (Bytes.to_string finaltext);
    let indexes = Array.of_list flatlist in
    let arrlen = Array.length indexes - 3 in
    let rec loop ticks idx =
      if ticks <= 0 then ()
      else begin
        print
          (Array.unsafe_get indexes idx)
          (Array.unsafe_get indexes (succ idx));
        Externs.caml_clock_nanosleep (Array.unsafe_get indexes (idx + 2));
        let nidx = if idx = arrlen then 0 else idx + 3 in
        (loop [@tailcall]) (pred ticks) nidx
      end
    in
    loop (pwlen * cycles) 0
  in
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
  let _, charlist = vclen_charlist (Bytes.to_string finaltext) in
  let revcharlist = List.rev charlist in
  let totallen = Bytes.length finaltext in
  let lenminuswidth =
    totallen - bytesofutfchars (String.concat charlist) width
  in
  let ucinds rev cl sptfn accfn =
    let rec loop pos acc = function
      | [] -> if rev then List.rev acc else acc
      | h :: t ->
          let lt = h :: t in
          let str = String.concat lt in
          let bts = bytesofutfchars str width in
          let l, r = List.split_n lt (sptfn lt) in
          loop (String.length (String.concat l) + pos) (accfn str bts pos acc) r
    in
    loop 0 [] cl
  in
  let wordsplitfn chr =
    succ (List.length (List.take_while chr ~f:(fun s -> String.( <> ) s " ")))
  in
  let accmfn _ bts pos acc = (pos, bts) :: acc in
  let brword =
    ucinds true charlist wordsplitfn (fun str bts _ acc ->
        (String.length str - bts, bts) :: acc)
  in
  let blchar = ucinds true revcharlist (fun _ -> 1) accmfn in
  let blword = ucinds true revcharlist wordsplitfn accmfn in
  let takeappend r l = List.append l (List.take r 1) in
  begin match (scroll_unit, Ordering.of_int (compare visual_chars width)) with
  | Char, (Greater | Equal | Less) ->
      let l, r = List.split_while blchar ~f:(fun (a, b) -> a + b < totallen) in
      List.append (takeappend r l) (List.rev (List.drop l 1)) |> loopandprint
  | Word, Greater -> begin
      let fltr =
        List.filteri (List.append blword brword) ~f:(fun i (a, _) ->
            (a > 0 || i = 0) && a <= lenminuswidth)
      in
      List.remove_consecutive_duplicates fltr ~equal:(fun (a, _) (b, _) ->
          a = b)
      |> loopandprint
    end
  | Word, (Equal | Less) ->
      [ (0, jointextlen + ecl); (lenminuswidth, jointextlen + ecl) ]
      |> loopandprint
  end;
  match terminator with
  | Newline -> ()
  | _ ->
      Externs.unsafe_output_char stdout '\n';
      Externs.unsafe_flush stdout
