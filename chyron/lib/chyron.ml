open Core

module Mode = struct
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
  type t = Left | Right | Bounce [@@deriving equal, sexp]

  let arg =
    Command.Arg_type.of_alist_exn ~accept_unique_prefixes:true
      ~case_sensitive:false ~list_values_in_help:false
      [ ("left", Left); ("right", Right); ("bounce", Bounce) ]
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

let bytesofutfchars str visualchars =
  let bytelen, _ =
    Uuseg_string.fold_utf_8 `Grapheme_cluster
      (fun (bytecount, charcount) char ->
        if charcount >= visualchars then (bytecount, charcount)
        else (bytecount + String.length char, succ charcount))
      (0, 0) str
  in
  bytelen

let bytesofutfcharsdiff str visualchars =
  let bytelen, chcnt =
    Uuseg_string.fold_utf_8 `Grapheme_cluster
      (fun (bytecount, charcount) char ->
        if charcount >= visualchars then (bytecount, charcount)
        else (bytecount + String.length char, succ charcount))
      (0, 0) str
  in
  bytelen - chcnt

let listofutfchars str =
  Uuseg_string.fold_utf_8 `Grapheme_cluster (fun acc char -> char :: acc) [] str

let runuc text
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
  let joined_text = String.concat ~sep:" " text in
  let text_len =
    Uuseg_string.fold_utf_8 `Grapheme_cluster
      (fun count _ -> succ count)
      0 joined_text
  in
  print_endline ("textlen " ^ string_of_int text_len);
  let width_minus_text_len = width - text_len in
  let ecl =
    if Direction.equal direction Bounce then Int.max 0 width_minus_text_len
    else
      Int.clamp_exn
        (Int.max endcap_len width_minus_text_len)
        ~min:1 ~max:(pred width)
  in
  let ecp = Bytes.make ecl endcap_char in
  let joined_bytes = Bytes.of_string joined_text in
  let bounce = Stdlib.Bytes.cat (Stdlib.Bytes.cat ecp joined_bytes) ecp in
  let finaltext, totallen =
    match direction with
    | Bounce -> (bounce, ecl + ecl + text_len)
    | Left ->
        (Stdlib.Bytes.cat joined_bytes bounce, ecl + ecl + text_len + text_len)
    | Right ->
        (Stdlib.Bytes.cat bounce joined_bytes, ecl + ecl + text_len + text_len)
  in

  let lastchar =
    match terminator with Newline -> '\n' | Return -> '\r' | Space -> ' '
  in
  (* let rec wordbounds th idx list f adj =
    if idx = th then list
    else if
      Char.( = ) (Bytes.unsafe_get finaltext idx) ' '
      && Char.( <> ) (Bytes.unsafe_get finaltext (succ idx)) ' '
    then (wordbounds [@tailcall]) th (f idx) ((idx - adj) :: list) f adj
    else (wordbounds [@tailcall]) th (f idx) list f adj
  in *)
  let pfix = Bytes.of_string prefix in
  let plen = Bytes.length pfix in
  let sfix = Bytes.of_string suffix in
  let slen = Bytes.length sfix in
  let print pos wid =
    print_endline (string_of_int pos ^ " " ^ string_of_int wid);
    Externs.unsafe_output_bytes stdout pfix 0 plen;
    Externs.unsafe_output_bytes stdout finaltext pos wid;
    Externs.unsafe_output_bytes stdout sfix 0 slen;
    Externs.unsafe_output_char stdout lastchar;
    Externs.unsafe_flush stdout
  in
  let ftstr = Bytes.to_string finaltext in
  let revstr = String.concat (listofutfchars ftstr) ^ "|\n" in
  let loopandprint ticks l =
    print_endline (List.to_string ~f:string_of_int l);
    print_endline (ftstr ^ "|\n");
    print_endline revstr;
    print_endline (string_of_int ticks);
    let indexes = Array.of_list l in
    Array.iter indexes ~f:(fun s -> print_endline (string_of_int s));
    let arrlen = pred (Array.length indexes - 1) in
    print_endline ("arrlen " ^ string_of_int arrlen);
    if rest = 0 then
      let rec loop ticks idx =
        print_endline ("ticks " ^ string_of_int ticks);
        print_endline ("idx " ^ string_of_int idx);
        if ticks <= 0 then ()
        else begin
          print
            (Array.unsafe_get indexes idx)
            (Array.unsafe_get indexes (idx + 1));
          Externs.caml_clock_nanosleep sleep;
          let nidx = if idx = arrlen then 0 else succ idx + 1 in
          (loop [@tailcall]) (pred ticks) nidx
        end
      in
      loop ticks 0
    else
      let rec minmax goal f = function
        | i :: lt -> minmax (if f i goal then i else goal) f lt
        | _ -> goal
      in
      let minidx = minmax 1024 ( < ) l in
      let maxidx = minmax 0 ( > ) l in
      let rec loop ticks idx =
        if ticks <= 0 then ()
        else begin
          let pos = Array.unsafe_get indexes idx in
          (* print_string (string_of_int pos ^ " "); *)
          print pos width;
          Externs.caml_clock_nanosleep
            (if pos = minidx || pos = maxidx then rest else sleep);
          let nidx = if idx = arrlen then 0 else succ idx in
          (loop [@tailcall]) (pred ticks) nidx
        end
      in
      loop ticks 0
  in
  let totallen = Bytes.length finaltext in
  let lenminuswidth = totallen - width - bytesofutfcharsdiff revstr width in
  let halflen = totallen asr 1 in
  (* let predlentext = pred totallen in *)
  let wordcount = List.fold text ~init:0 ~f:(fun i _ -> succ i) in
  (* let revandtake n l = List.take (List.rev l) n in *)

  print_endline (string_of_int totallen);
  print_endline (string_of_int lenminuswidth);
  print_endline (string_of_int halflen);

  let rec getinx tix charlist lt =
    print_endline (List.to_string ~f:(fun x -> x) charlist);
    if tix <= 0 then lt
    else
      let f = String.concat charlist in
      let b = bytesofutfchars f width in
      print_endline (string_of_int b);
      getinx (pred tix)
        (List.drop
           (List.drop_while charlist ~f:(fun x -> String.( <> ) x " "))
           1)
        (b :: (String.length f - b) :: lt)
  in

  let rec tupelize acc = function
    | h :: n :: t -> tupelize ((h, n) :: acc) t
    | _ -> List.rev acc
  in

  let rec detupelize acc = function
    | (a, b) :: t -> detupelize (b :: a :: acc) t
    | _ -> List.rev acc
  in

  let rec getinxl tix charlist drops lt =
    print_endline (List.to_string ~f:(fun x -> x) charlist);
    if tix <= 0 then lt
    else
      let f = String.concat charlist in
      let b = bytesofutfchars f width in
      print_endline (string_of_int b);
      let l, r = List.split_while charlist ~f:(fun x -> String.( <> ) x " ") in
      getinxl (pred tix) (List.drop r 1)
        (succ (String.length (String.concat l)) + drops)
        (b :: drops :: lt)
  in

  let rec getinxlch charlist drops lt =
    if drops >= succ lenminuswidth then lt
    else
      let f = String.concat charlist in
      let b = bytesofutfchars f width in
      let l, r = List.split_n charlist 1 in
      getinxlch r (String.length (String.concat l) + drops) (b :: drops :: lt)
  in

  begin match
    (direction, scroll, mode, Ordering.of_int (compare text_len width))
  with
  | Bounce, Word, (Wrap | Reset), Greater -> begin
      (* let bwordcount = Int.max 1 (pred wordcount) in *)
      let ggg = getinx wordcount (listofutfchars ftstr) [] in
      let p = tupelize [] (List.rev ggg) in
      print_endline
        (List.to_string
           ~f:(fun (a, b) -> string_of_int a ^ "," ^ string_of_int b)
           p);
      let gggw = getinxl wordcount (List.rev (listofutfchars ftstr)) 0 [] in
      let pl = tupelize [] (List.rev gggw) in
      print_endline
        (List.to_string
           ~f:(fun (a, b) -> string_of_int a ^ "," ^ string_of_int b)
           pl);

      let fltr =
        List.filteri (List.append pl p) ~f:(fun i (x, _) ->
            (x > 0 || i = 0) && x <= lenminuswidth)
      in
      print_endline
        ("fltr: "
        ^ List.to_string
            ~f:(fun (a, b) -> string_of_int a ^ "," ^ string_of_int b)
            fltr);
      let indexes =
        detupelize []
          (List.remove_consecutive_duplicates fltr ~equal:(fun (a, _) (b, _) ->
               a = b))
      in
      print_endline
        ("inds: " ^ List.to_string ~f:(fun x -> string_of_int x) indexes);
      loopandprint (List.length indexes / 2 * cycles) indexes
      (* let lh =
        revandtake bwordcount (wordbounds predlentext 1 [ 0 ] succ (-1))
      in
      let rh =
        revandtake bwordcount
          (wordbounds width predlentext [ lenminuswidth ] pred width)
      in
      let fltr =
        List.filter (List.append lh rh) ~f:(fun x -> x <= lenminuswidth)
      in
      let indexes =
        List.remove_consecutive_duplicates fltr ~equal:(fun a b -> a = b)
      in
      loopandprint (List.length indexes * cycles) indexes *)
    end
  | Right, Word, Reset, Greater -> begin
      let ggg = getinx wordcount (listofutfchars ftstr) [] in
      let p =
        detupelize []
          (List.filter
             (tupelize [] (List.rev ggg))
             ~f:(fun (x, _) -> x >= halflen))
      in
      print_endline (List.to_string ~f:string_of_int p);
      print_endline "-----";
      p
      (* revandtake wordcount
        (wordbounds width predlentext [ lenminuswidth ] pred width) *)
      |> loopandprint (List.length p / 2 * cycles)
      (* let prelims =
        revandtake wordcount
          (wordbounds width predlentext [ lenminuswidth ] pred width)
      in
      let los =
        List.fold prelims ~init:(-1) ~f:(fun a x ->
            if x <= succ halflen then succ a else a)
      in
      let indexes = List.length prelims - los |> List.take prelims in
      loopandprint (List.length indexes * cycles) indexes *)
    end
  | Left, Word, Reset, Greater -> begin
      let ggg = getinxl wordcount (List.rev (listofutfchars ftstr)) 0 [] in
      let p, q =
        (* detupelize [] *)
        List.split_while
          (tupelize [] (List.rev ggg))
          ~f:(fun (x, y) -> x + y < halflen)
      in
      let r = List.take q 1 in
      print_endline (List.to_string ~f:string_of_int (detupelize [] p));
      print_endline (List.to_string ~f:string_of_int (detupelize [] r));
      print_endline (string_of_int (bytesofutfchars revstr width));
      print_endline "-----";
      detupelize [] (List.append p r)
      (* revandtake wordcount
        (wordbounds width predlentext [ lenminuswidth ] pred width) *)
      |> loopandprint (List.length (List.append p r) * cycles)
      (* let prelims =
        revandtake wordcount (wordbounds halflen 0 [ 0 ] succ (-1))
      in
      let his =
        List.fold prelims ~init:(-1) ~f:(fun a x ->
            if x >= pred (halflen - width) then succ a else a)
      in
      let indexes = List.length prelims - his |> List.take prelims in
      loopandprint (List.length indexes * cycles) indexes *)
    end
  | Right, Word, Wrap, (Greater | Equal | Less) ->
      let ggg = getinx wordcount (listofutfchars ftstr) [] in
      let p = List.rev ggg in
      print_endline (List.to_string ~f:string_of_int p);
      print_endline "-----";
      p
      (* revandtake wordcount
        (wordbounds width predlentext [ lenminuswidth ] pred width) *)
      |> loopandprint (wordcount * cycles)
  | Left, Word, Wrap, (Greater | Equal | Less) ->
      let ggg = getinxl wordcount (List.rev (listofutfchars ftstr)) 0 [] in
      let p = List.rev ggg in
      print_endline (List.to_string ~f:string_of_int p);
      print_endline "-----";
      p
      (* revandtake wordcount (wordbounds halflen 0 [ 0 ] succ (-1)) *)
      |> loopandprint (wordcount * cycles)
  | Bounce, Char, (Wrap | Reset), (Greater | Equal | Less) ->
      let o = getinxlch (List.rev (listofutfchars ftstr)) 0 [] in
      let _, r = List.split_n o 2 in
      let _, r2 = List.split_n (List.rev r) 2 in
      let f = List.rev_append o (detupelize [] (List.rev (tupelize [] r2))) in

      (* (let rh = List.range ~stride:(-1) ~start:`exclusive lenminuswidth 0 in
       0 :: List.rev_append rh (lenminuswidth :: rh)) *)
      loopandprint (List.length f / 2 * cycles) f
  | Right, Char, Wrap, (Greater | Equal | Less) ->
      List.range ~stride:(-1) lenminuswidth (lenminuswidth - halflen)
      |> loopandprint (halflen * cycles)
  | Right, Char, Reset, Greater ->
      List.range ~stride:(-1) lenminuswidth halflen
      |> loopandprint (succ (text_len - width) * cycles)
  | Left, Char, Reset, Greater ->
      List.range 0 (halflen - width)
      |> loopandprint (succ (text_len - width) * cycles)
  | Left, Char, Wrap, (Greater | Equal | Less) ->
      List.range 0 halflen |> loopandprint (halflen * cycles)
  | Bounce, Word, (Wrap | Reset), (Equal | Less)
  | (Left | Right), (Char | Word), Reset, (Equal | Less) ->
      loopandprint (cycles lsl 1) [ 0; lenminuswidth ]
  end;
  match terminator with
  | Newline -> ()
  | _ ->
      Externs.unsafe_output_char stdout '\n';
      Externs.unsafe_flush stdout

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
  let rec textlen acc = function
    | [] -> acc
    | str :: rest -> (textlen [@tailcall]) (succ acc + String.length str) rest
  in
  let text_len = textlen (-1) text in
  print_endline ("textlen" ^ string_of_int text_len);
  let width_minus_text_len = width - text_len in
  let ecl =
    if Direction.equal direction Bounce then Int.max 0 width_minus_text_len
    else
      Int.clamp_exn
        (Int.max endcap_len width_minus_text_len)
        ~min:1 ~max:(pred width)
  in
  let half_len = text_len + ecl in
  let total_len =
    match direction with
    | Bounce -> ecl + half_len
    | Left | Right -> half_len lsl 1
  in
  let rec blittext ~dst pos = function
    | [] -> ()
    | s :: ts -> (
        let len = String.length s in
        let poslen = pos + len in
        Bytes.From_string.blit ~src:s ~src_pos:0 ~dst ~dst_pos:pos ~len;
        match ts with
        | [] -> ()
        | _ ->
            Bytes.set dst poslen ' ';
            (blittext [@tailcall]) ~dst (succ poslen) ts)
  in
  let finaltext = Bytes.create total_len in
  begin match direction with
  | Bounce ->
      Bytes.fill finaltext ~pos:0 ~len:ecl endcap_char;
      blittext ~dst:finaltext ecl text;
      Bytes.fill finaltext ~pos:(ecl + text_len) ~len:ecl endcap_char
  | Left ->
      blittext ~dst:finaltext 0 text;
      Bytes.fill finaltext ~pos:text_len ~len:ecl endcap_char;
      Bytes.blit ~src:finaltext ~src_pos:0 ~dst:finaltext ~dst_pos:half_len
        ~len:half_len
  | Right ->
      Bytes.fill finaltext ~pos:0 ~len:ecl endcap_char;
      blittext ~dst:finaltext ecl text;
      Bytes.blit ~src:finaltext ~src_pos:0 ~dst:finaltext ~dst_pos:half_len
        ~len:half_len
  end;
  let lastchar =
    match terminator with Newline -> '\n' | Return -> '\r' | Space -> ' '
  in
  let rec wordbounds th idx list f adj =
    if idx = th then list
    else if
      Char.( = ) (Bytes.unsafe_get finaltext idx) ' '
      && Char.( <> ) (Bytes.unsafe_get finaltext (succ idx)) ' '
    then (wordbounds [@tailcall]) th (f idx) ((idx - adj) :: list) f adj
    else (wordbounds [@tailcall]) th (f idx) list f adj
  in
  let pfix = Bytes.of_string prefix in
  let plen = Bytes.length pfix in
  let sfix = Bytes.of_string suffix in
  let slen = Bytes.length sfix in
  let print pos =
    Externs.unsafe_output_bytes stdout pfix 0 plen;
    Externs.unsafe_output_bytes stdout finaltext pos width;
    Externs.unsafe_output_bytes stdout sfix 0 slen;
    Externs.unsafe_output_char stdout lastchar;
    Externs.unsafe_flush stdout
  in
  let loopandprint ticks l =
    print_endline (List.to_string ~f:string_of_int l);
    print_endline (Bytes.to_string finaltext ^ "|\n");
    let indexes = Array.of_list l in
    let arrlen = pred (Array.length indexes) in
    if rest = 0 then
      let rec loop ticks idx =
        if ticks <= 0 then ()
        else begin
          print (Array.unsafe_get indexes idx);
          Externs.caml_clock_nanosleep sleep;
          let nidx = if idx = arrlen then 0 else succ idx in
          (loop [@tailcall]) (pred ticks) nidx
        end
      in
      loop ticks 0
    else
      let rec minmax goal f = function
        | i :: lt -> minmax (if f i goal then i else goal) f lt
        | _ -> goal
      in
      let minidx = minmax 1024 ( < ) l in
      let maxidx = minmax 0 ( > ) l in
      let rec loop ticks idx =
        if ticks <= 0 then ()
        else begin
          let pos = Array.unsafe_get indexes idx in
          (* print_string (string_of_int pos ^ " "); *)
          print pos;
          Externs.caml_clock_nanosleep
            (if pos = minidx || pos = maxidx then rest else sleep);
          let nidx = if idx = arrlen then 0 else succ idx in
          (loop [@tailcall]) (pred ticks) nidx
        end
      in
      loop ticks 0
  in
  let lenminuswidth = total_len - width in
  let halflen = total_len asr 1 in
  let predlentext = pred total_len in
  let wordcount = List.fold text ~init:0 ~f:(fun i _ -> succ i) in
  let revandtake n l = List.take (List.rev l) n in

  print_endline (string_of_int total_len);
  print_endline (string_of_int lenminuswidth);
  print_endline (string_of_int halflen);
  begin match
    (direction, scroll, mode, Ordering.of_int (compare text_len width))
  with
  | Bounce, Word, (Wrap | Reset), Greater -> begin
      let lh =
        revandtake wordcount (wordbounds predlentext 1 [ 0 ] succ (-1))
      in
      let rh =
        revandtake wordcount
          (wordbounds width predlentext [ lenminuswidth ] pred width)
      in
      print_endline
        (List.to_string ~f:string_of_int
           (wordbounds predlentext 1 [ 0 ] succ (-1)));
      print_endline
        (List.to_string ~f:string_of_int
           (wordbounds width predlentext [ lenminuswidth ] pred width));
      let fltr =
        List.filter (List.append lh rh) ~f:(fun x -> x <= lenminuswidth)
      in
      let indexes =
        List.remove_consecutive_duplicates fltr ~equal:(fun a b -> a = b)
      in
      loopandprint (List.length indexes * cycles) indexes
    end
  | Right, Word, Reset, Greater -> begin
      let prelims =
        revandtake wordcount
          (wordbounds width predlentext [ lenminuswidth ] pred width)
      in
      let los =
        List.fold prelims ~init:(-1) ~f:(fun a x ->
            if x <= succ halflen then succ a else a)
      in
      let indexes = List.length prelims - los |> List.take prelims in
      loopandprint (List.length indexes * cycles) indexes
    end
  | Left, Word, Reset, Greater -> begin
      let prelims =
        revandtake wordcount (wordbounds halflen 0 [ 0 ] succ (-1))
      in
      let his =
        List.fold prelims ~init:(-1) ~f:(fun a x ->
            if x >= pred (halflen - width) then succ a else a)
      in
      let indexes = List.length prelims - his |> List.take prelims in
      loopandprint (List.length indexes * cycles) indexes
    end
  | Right, Word, Wrap, (Greater | Equal | Less) ->
      revandtake wordcount
        (wordbounds width predlentext [ lenminuswidth ] pred width)
      |> loopandprint (wordcount * cycles)
  | Left, Word, Wrap, (Greater | Equal | Less) ->
      revandtake wordcount (wordbounds halflen 0 [ 0 ] succ (-1))
      |> loopandprint (wordcount * cycles)
  | Bounce, Char, (Wrap | Reset), (Greater | Equal | Less) ->
      (let rh = List.range ~stride:(-1) ~start:`exclusive lenminuswidth 0 in
       0 :: List.rev_append rh (lenminuswidth :: rh))
      |> loopandprint ((Int.max 1 lenminuswidth * cycles) lsl 1)
  | Right, Char, Wrap, (Greater | Equal | Less) ->
      List.range ~stride:(-1) lenminuswidth (lenminuswidth - halflen)
      |> loopandprint (halflen * cycles)
  | Right, Char, Reset, Greater ->
      List.range ~stride:(-1) lenminuswidth halflen
      |> loopandprint (succ (text_len - width) * cycles)
  | Left, Char, Reset, Greater ->
      List.range 0 (halflen - width)
      |> loopandprint (succ (text_len - width) * cycles)
  | Left, Char, Wrap, (Greater | Equal | Less) ->
      List.range 0 halflen |> loopandprint (halflen * cycles)
  | Bounce, Word, (Wrap | Reset), (Equal | Less)
  | (Left | Right), (Char | Word), Reset, (Equal | Less) ->
      loopandprint (cycles lsl 1) [ 0; lenminuswidth ]
  end;
  match terminator with
  | Newline -> ()
  | _ ->
      Externs.unsafe_output_char stdout '\n';
      Externs.unsafe_flush stdout
