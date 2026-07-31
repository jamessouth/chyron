open Core

module Mode = struct
  type t = Reset | Wrap [@@deriving sexp]

  let arg =
    Command.Arg_type.of_alist_exn ~accept_unique_prefixes:true
      ~case_sensitive:false ~list_values_in_help:false
      [ ("reset", Reset); ("wrap", Wrap) ]
end

module Ints = struct
  let getint ~min num =
    if min |> Int.is_negative then invalid_arg "min must be >= 0"
    else
      match num |> int_of_string_opt with
      | Some n -> Int.max min n
      | None -> invalid_arg "not an int"

  let nonneg = Command.Arg_type.create (getint ~min:0)
  let oneplus = Command.Arg_type.create (getint ~min:1)
  let twoplus = Command.Arg_type.create (getint ~min:2)
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

let rec gettextlen acc = function
  | [] -> acc
  | str :: rest -> gettextlen (succ acc + String.length str) rest

let getfinaltext text endcap_char endcap_len width direction mode =
  let text_len = gettextlen (-1) text in
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
            blittext ~dst (succ poslen) ts)
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
  let finaltext =
    getfinaltext text endcap_char endcap_len width direction mode
  in
  let lastchar =
    match terminator with Newline -> '\n' | Return -> '\r' | Space -> ' '
  in
  let rec getwordbounds th idx list f adj =
    if idx = th then list
    else if
      Char.( = ) (Bytes.unsafe_get finaltext idx) ' '
      && Char.( <> ) (Bytes.unsafe_get finaltext (succ idx)) ' '
    then getwordbounds th (f idx) ((idx - adj) :: list) f adj
    else getwordbounds th (f idx) list f adj
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
  let loopandprint ticks indexes =
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
  let lenorigtext = gettextlen (-1) text in

  begin match
    (direction, scroll, mode, Ordering.of_int (compare lenorigtext width))
  with
  | Bounce, Char, _, _ -> begin
      let lenminuswidth = lenfintext - width in
      (let rh = List.range ~stride:(-1) ~start:`exclusive lenminuswidth 0 in
       0 :: List.rev_append rh (lenminuswidth :: rh))
      |> Array.of_list
      |> loopandprint ((Int.max 1 lenminuswidth * cycles) lsl 1)
    end
  | Bounce, Word, _, Greater -> begin
      let lenminuswidth = lenfintext - width in
      let indexes =
        let wordcount =
          Int.max 1 (pred (List.fold text ~init:0 ~f:(fun i _ -> succ i)))
        in
        List.remove_consecutive_duplicates
          (List.filter
             (List.append
                (wordcount
                |> List.take
                     (getwordbounds (pred lenfintext) 1 [ 0 ] succ (-1)
                     |> List.rev))
                (wordcount
                |> List.take
                     (getwordbounds width (pred lenfintext) [ lenminuswidth ]
                        pred width
                     |> List.rev)))
             ~f:(fun x -> x <= lenminuswidth))
          ~equal:(fun a b -> a = b)
        |> Array.of_list
      in
      loopandprint (Array.length indexes * cycles) indexes
    end
  | Bounce, Word, _, Equal -> begin [| 0 |] |> loopandprint cycles end
  | Bounce, Word, _, Less -> begin
      let lenminuswidth = lenfintext - width in
      [| 0; lenminuswidth |] |> loopandprint (2 * cycles)
    end
  | Left, Char, Wrap, _ -> begin
      let halflen = lenfintext asr 1 in
      List.range 0 halflen |> Array.of_list |> loopandprint (halflen * cycles)
    end
  | Left, Char, Reset, Greater -> begin
      let halflen = lenfintext asr 1 in

      List.range 0 (halflen - width)
      |> Array.of_list
      |> loopandprint (succ (lenorigtext - width) * cycles)
    end
  | Left, Char, Reset, (Equal | Less) -> begin [| 0 |] |> loopandprint cycles end
  | Left, Word, Wrap, _ -> begin
      let halflen = lenfintext asr 1 in
      let wordcount = List.fold text ~init:0 ~f:(fun i _ -> succ i) in
      wordcount
      |> List.take (getwordbounds halflen 0 [ 0 ] succ (-1) |> List.rev)
      |> Array.of_list
      |> loopandprint (wordcount * cycles)
    end
  | Left, Word, Reset, Greater -> begin
      let halflen = lenfintext asr 1 in

      let wordcount = List.fold text ~init:0 ~f:(fun i _ -> succ i) in
      let prelims =
        wordcount
        |> List.take (getwordbounds halflen 0 [ 0 ] succ (-1) |> List.rev)
      in
      let his =
        List.fold prelims ~init:(-1) ~f:(fun a x ->
            if x >= pred (halflen - width) then succ a else a)
      in
      let indexes =
        List.length prelims - his |> List.take prelims |> Array.of_list
      in
      loopandprint (Array.length indexes * cycles) indexes
    end
  | Left, Word, Reset, _ -> begin [| 0 |] |> loopandprint cycles end
  | Right, Char, Wrap, _ -> begin
      let lenminuswidth = lenfintext - width in
      let halflen = lenfintext asr 1 in
      let minpos = lenminuswidth - halflen in
      List.range ~stride:(-1) lenminuswidth minpos
      |> Array.of_list
      |> loopandprint (halflen * cycles)
    end
  | Right, Char, Reset, Greater -> begin
      let lenminuswidth = lenfintext - width in
      let halflen = lenfintext asr 1 in

      List.range ~stride:(-1) lenminuswidth halflen
      |> Array.of_list
      |> loopandprint (succ (lenorigtext - width) * cycles)
    end
  | Right, Char, Reset, _ -> begin
      let lenminuswidth = lenfintext - width in

      [| lenminuswidth |] |> loopandprint cycles
    end
  | Right, Word, Wrap, _ -> begin
      let lenminuswidth = lenfintext - width in
      let wordcount = List.fold text ~init:0 ~f:(fun i _ -> succ i) in
      wordcount
      |> List.take
           (getwordbounds width (pred lenfintext) [ lenminuswidth ] pred width
           |> List.rev)
      |> Array.of_list
      |> loopandprint (wordcount * cycles)
    end
  | Right, Word, Reset, Greater -> begin
      let lenminuswidth = lenfintext - width in
      let halflen = lenfintext asr 1 in

      let wordcount = List.fold text ~init:0 ~f:(fun i _ -> succ i) in
      let prelims =
        wordcount
        |> List.take
             (getwordbounds width (pred lenfintext) [ lenminuswidth ] pred width
             |> List.rev)
      in
      let lows =
        List.fold prelims ~init:(-1) ~f:(fun a x ->
            if x <= succ halflen then succ a else a)
      in
      let indexes =
        List.take prelims (List.length prelims - lows) |> Array.of_list
      in
      loopandprint (Array.length indexes * cycles) indexes
    end
  | Right, Word, Reset, _ -> begin
      let lenminuswidth = lenfintext - width in

      [| lenminuswidth |] |> loopandprint cycles
    end
  end;
  match terminator with
  | Newline -> ()
  | _ ->
      Externs.unsafe_output_char stdout '\n';
      Externs.unsafe_flush stdout
