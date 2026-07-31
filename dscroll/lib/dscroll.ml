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
    if min |> Int.is_negative then invalid_arg "min must be >= 0"
    else
      match num |> int_of_string_opt with
      | Some n -> Int.max min n
      | None -> invalid_arg "not an int"

  let nonneg = Command.Arg_type.create (parseint ~min:0)
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
  (* initial_pause : int; *)
  mode : Mode.t;
  prefix : string;
  scroll : Scroll.t;
  sleep : int;
  suffix : string;
  terminator : Terminator.t;
  width : int;
}

let rec textlen acc = function
  | [] -> acc
  | str :: rest -> (textlen [@tailcall]) (succ acc + String.length str) rest

let finaltext text endcap_char endcap_len width direction =
  let text_len = textlen (-1) text in
  let width_minus_text_len = width - text_len in
  let ecl =
    if Direction.equal direction Bounce then Int.max 0 width_minus_text_len
    else
      Int.clamp_exn
        (Int.max endcap_len width_minus_text_len)
        ~min:1 ~max:(pred width)
  in
  let halflen = text_len + ecl in
  let total_len =
    match direction with
    | Bounce -> ecl + halflen
    | Left | Right -> halflen lsl 1
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
  let buf = Bytes.create total_len in
  begin match direction with
  | Bounce ->
      Bytes.fill buf ~pos:0 ~len:ecl endcap_char;
      blittext ~dst:buf ecl text;
      Bytes.fill buf ~pos:(ecl + text_len) ~len:ecl endcap_char
  | Left ->
      blittext ~dst:buf 0 text;
      Bytes.fill buf ~pos:text_len ~len:ecl endcap_char;
      Bytes.blit ~src:buf ~src_pos:0 ~dst:buf ~dst_pos:halflen ~len:halflen
  | Right ->
      Bytes.fill buf ~pos:0 ~len:ecl endcap_char;
      blittext ~dst:buf ecl text;
      Bytes.blit ~src:buf ~src_pos:0 ~dst:buf ~dst_pos:halflen ~len:halflen
  end;
  buf

let run text
    {
      cycles;
      direction;
      endcap_char;
      endcap_len;
      (* initial_pause; *)
      mode;
      prefix;
      scroll;
      sleep;
      suffix;
      terminator;
      width;
    } =
  let finaltext = finaltext text endcap_char endcap_len width direction in
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
    let indexes = Array.of_list l in
    let maxidx = pred (Array.length indexes) in
    let rec loop ticks idx =
      if ticks <= 0 then ()
      else begin
        print (Array.unsafe_get indexes idx);
        let nidx = if idx = maxidx then 0 else succ idx in
        Externs.caml_clock_nanosleep sleep;
        (loop [@tailcall]) (pred ticks) nidx
      end
    in
    loop ticks 0
  in
  let lenfintext = Bytes.length finaltext in
  let lenminuswidth = lenfintext - width in
  let halflen = lenfintext asr 1 in
  let predlentext = pred lenfintext in
  let wordcount = List.fold text ~init:0 ~f:(fun i _ -> succ i) in
  let lenorigtext = textlen (-1) text in
  let revandtake n l = List.take (List.rev l) n in

  begin match
    (direction, scroll, mode, Ordering.of_int (compare lenorigtext width))
  with
  | Bounce, Word, (Wrap | Reset), Greater -> begin
      let bwordcount = Int.max 1 (pred wordcount) in
      let lh =
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
      |> loopandprint (succ (lenorigtext - width) * cycles)
  | Left, Char, Reset, Greater ->
      List.range 0 (halflen - width)
      |> loopandprint (succ (lenorigtext - width) * cycles)
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
