open Core

module Mode = struct
  type t = Reset | Split_flap | Wrap [@@deriving sexp]

  let arg =
    Command.Arg_type.of_alist_exn ~accept_unique_prefixes:true
      ~case_sensitive:false ~list_values_in_help:false
      [ ("reset", Reset); ("split-flap", Split_flap); ("wrap", Wrap) ]
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

module Scroll = struct
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
  type t = Bounce | Left | Right [@@deriving equal, sexp]

  let arg =
    Command.Arg_type.of_alist_exn ~accept_unique_prefixes:true
      ~case_sensitive:false ~list_values_in_help:false
      [ ("bounce", Bounce); ("left", Left); ("right", Right) ]
end

type cliflags = {
  cycles : int;
  direction : Direction.t;
  endcap_char : char;
  endcap_len : int;
  rest : int;
  mode : Mode.t;
  prefix : string;
  scroll : Scroll.t;
  sleep : int;
  suffix : string;
  terminator : Terminator.t;
  width : int;
}

let run text
    {
      cycles;
      direction;
      endcap_char;
      endcap_len;
      rest;
      mode;
      prefix;
      scroll;
      sleep;
      suffix;
      terminator;
      width;
    } =
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
    if Direction.equal direction Bounce then Int.max 0 width_minus_visual_chars
    else
      Int.clamp_exn
        (Int.max endcap_len width_minus_visual_chars)
        ~min:1 ~max:(pred width)
  in
  let ecp = Bytes.make ecl endcap_char in
  let joined_bytes = Bytes.of_string joined_text in
  let bounce = Stdlib.Bytes.cat (Stdlib.Bytes.cat ecp joined_bytes) ecp in
  let finaltext =
    match direction with
    | Bounce -> bounce
    | Left -> Stdlib.Bytes.cat joined_bytes bounce
    | Right -> Stdlib.Bytes.cat bounce joined_bytes
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
  (* this is buggy for bounce and wrap modes when rest times are used -  
  the rest isn't placed on the bounce extreme. it's also repeated in wrap since last abuts first*)
  let loopandprint pwlist =
    let len = List.length pwlist in
    let tot = sleep + rest in
    let slist =
      match len with
      | 1 -> [ tot ]
      | 2 -> [ tot; tot ]
      | _ ->
          let rec loop i acc =
            if i = 0 then tot :: acc else loop (pred i) (sleep :: acc)
          in
          loop (len - 2) [ tot ]
    in
    let flatlist =
      let rec loop pwl sll =
        match (pwl, sll) with
        | [], [] -> []
        | (p, w) :: pw, s :: sl -> p :: w :: s :: loop pw sl
        | _, _ -> []
      in
      loop pwlist slist
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
    loop (len * cycles) 0
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
    (direction, scroll, mode, Ordering.of_int (compare visual_chars width))
  with
  | _, _, Split_flap, _ -> print_endline "split-flap"
  | Bounce, Char, (Wrap | Reset), (Greater | Equal | Less) ->
      let l, r = List.split_while blchar ~f:(fun (a, b) -> a + b < totallen) in
      List.append (takeappend r l) (List.rev (List.drop l 1)) |> loopandprint
  | Bounce, Word, (Wrap | Reset), Greater -> begin
      let fltr =
        List.filteri (List.append blword brword) ~f:(fun i (a, _) ->
            (a > 0 || i = 0) && a <= lenminuswidth)
      in
      List.remove_consecutive_duplicates fltr ~equal:(fun (a, _) (b, _) ->
          a = b)
      |> loopandprint
    end
  | Bounce, Word, (Wrap | Reset), (Equal | Less) ->
      [ (0, jointextlen + ecl); (lenminuswidth, jointextlen + ecl) ]
      |> loopandprint
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
      List.take blword (List.fold text ~init:0 ~f:(fun i _ -> succ i))
      |> loopandprint
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
      List.take brword (List.fold text ~init:0 ~f:(fun i _ -> succ i))
      |> loopandprint
  end;
  match terminator with
  | Newline -> ()
  | _ ->
      Externs.unsafe_output_char stdout '\n';
      Externs.unsafe_flush stdout
