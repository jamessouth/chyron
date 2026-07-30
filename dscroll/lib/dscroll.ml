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

  let print pfix plen finaltext width sfix slen lastchar pos =
    unsafe_output_bytes stdout pfix 0 plen;
    unsafe_output_bytes stdout finaltext pos width;
    unsafe_output_bytes stdout sfix 0 slen;
    unsafe_output_char stdout lastchar;
    unsafe_flush stdout
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

let loopandprint ticks prefix finaltext width suffix lastchar sleep indexes =
  let pfix = Bytes.of_string prefix in
  let plen = Bytes.length pfix in
  let sfix = Bytes.of_string suffix in
  let slen = Bytes.length sfix in
  let maxidx = pred (Array.length indexes) in
  let rec loop ticks idx =
    if ticks <= 0 then ()
    else begin
      Externs.print pfix plen finaltext width sfix slen lastchar
        (Array.unsafe_get indexes idx);
      Externs.caml_clock_nanosleep sleep;
      (loop [@tailcall]) (pred ticks) (if idx = maxidx then 0 else succ idx)
    end
  in
  loop ticks 0

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
  let lentext = Bytes.length finaltext in
  let lastchar =
    (* cli help info doesn't look right so these chars are here *)
    match terminator with
    | Newline -> '\n'
    | Return -> '\r'
    | Space -> ' '
  in
  (* reset mode doesn't make sense with bounce *)

  begin match scroll with
  | Char ->
      begin match (direction, mode) with
      | Bounce, _ -> begin
          let lenminuswidth = lentext - width in
          (let rh = List.range ~stride:(-1) ~start:`exclusive lenminuswidth 0 in
           0 :: List.rev_append rh (lenminuswidth :: rh))
          |> Array.of_list
          |> loopandprint
               ((Int.max 1 lenminuswidth * cycles) lsl 1)
               prefix finaltext width suffix lastchar sleep
        end
      | Left, Wrap -> begin
          let halflen = lentext asr 1 in
          List.range 0 halflen |> Array.of_list
          |> loopandprint (halflen * cycles) prefix finaltext width suffix
               lastchar sleep
        end
      | Left, Reset -> begin
          let halflen = lentext asr 1 in

          let text_len = gettextlen (-1) text in
          if text_len > width then
            List.range 0 (halflen - width)
            |> Array.of_list
            |> loopandprint
                 (succ (text_len - width) * cycles)
                 prefix finaltext width suffix lastchar sleep
          else
            [| 0 |]
            |> loopandprint cycles prefix finaltext width suffix lastchar sleep
        end
      | Right, Wrap -> begin
          let lenminuswidth = lentext - width in
          let halflen = lentext asr 1 in
          let minpos = lenminuswidth - halflen in
          List.range ~stride:(-1) lenminuswidth minpos
          |> Array.of_list
          |> loopandprint (halflen * cycles) prefix finaltext width suffix
               lastchar sleep
        end
      | Right, Reset -> begin
          let lenminuswidth = lentext - width in
          let halflen = lentext asr 1 in
          let text_len = gettextlen (-1) text in
          let minpos = halflen in
          if text_len > width then
            List.range ~stride:(-1) lenminuswidth minpos
            |> Array.of_list
            |> loopandprint
                 (succ (text_len - width) * cycles)
                 prefix finaltext width suffix lastchar sleep
          else
            [| lenminuswidth |]
            |> loopandprint cycles prefix finaltext width suffix lastchar sleep
        end
      end
  | Word ->
      let rec getwordbounds th byt idx list f adj =
        if idx = th then list
        else if
          Char.( = ) (Bytes.unsafe_get byt idx) ' '
          && Char.( <> ) (Bytes.unsafe_get byt (succ idx)) ' '
        then getwordbounds th byt (f idx) ((idx - adj) :: list) f adj
        else getwordbounds th byt (f idx) list f adj
      in

      begin match (direction, mode) with
      | Bounce, _ -> begin
          let lenminuswidth = lentext - width in

          let text_len = gettextlen (-1) text in
          let indexes =
            (if text_len > width then
               let wordcount =
                 Int.max 1
                   (pred (List.fold text ~init:0 ~f:(fun i _ -> succ i)))
               in
               List.remove_consecutive_duplicates
                 (List.filter
                    (List.append
                       (wordcount
                       |> List.take
                            (getwordbounds
                               (pred (Bytes.length finaltext))
                               finaltext 1 [ 0 ] succ (-1)
                            |> List.rev))
                       (wordcount
                       |> List.take
                            (getwordbounds width finaltext (pred lentext)
                               [ lenminuswidth ] pred width
                            |> List.rev)))
                    ~f:(fun x -> x <= lenminuswidth))
                 ~equal:(fun a b -> a = b)
             else if text_len = width then [ 0 ]
             else [ 0; lenminuswidth ])
            |> Array.of_list
          in
          loopandprint
            (Array.length indexes * cycles)
            prefix finaltext width suffix lastchar sleep indexes
        end
      | Left, Wrap -> begin
          let halflen = lentext asr 1 in

          let wordcount = List.fold text ~init:0 ~f:(fun i _ -> succ i) in
          wordcount
          |> List.take
               (getwordbounds halflen finaltext 0 [ 0 ] succ (-1) |> List.rev)
          |> Array.of_list
          |> loopandprint (wordcount * cycles) prefix finaltext width suffix
               lastchar sleep
        end
      | Left, Reset -> begin
          let halflen = lentext asr 1 in

          let text_len = gettextlen (-1) text in
          if text_len > width then
            let wordcount = List.fold text ~init:0 ~f:(fun i _ -> succ i) in
            let prelims =
              wordcount
              |> List.take
                   (getwordbounds halflen finaltext 0 [ 0 ] succ (-1)
                   |> List.rev)
            in
            let his =
              List.fold prelims ~init:(-1) ~f:(fun a x ->
                  if x >= pred (halflen - width) then succ a else a)
            in
            let indexes =
              List.length prelims - his |> List.take prelims |> Array.of_list
            in
            loopandprint
              (Array.length indexes * cycles)
              prefix finaltext width suffix lastchar sleep indexes
          else
            [| 0 |]
            |> loopandprint cycles prefix finaltext width suffix lastchar sleep
        end
      | Right, Wrap -> begin
          let lenminuswidth = lentext - width in

          let wordcount = List.fold text ~init:0 ~f:(fun i _ -> succ i) in
          wordcount
          |> List.take
               (getwordbounds width finaltext (pred lentext) [ lenminuswidth ]
                  pred width
               |> List.rev)
          |> Array.of_list
          |> loopandprint (wordcount * cycles) prefix finaltext width suffix
               lastchar sleep
        end
      | Right, Reset -> begin
          let lenminuswidth = lentext - width in
          let halflen = lentext asr 1 in

          let text_len = gettextlen (-1) text in
          if text_len > width then
            let wordcount = List.fold text ~init:0 ~f:(fun i _ -> succ i) in
            let prelims =
              wordcount
              |> List.take
                   (getwordbounds width finaltext (pred lentext)
                      [ lenminuswidth ] pred width
                   |> List.rev)
            in
            let lows =
              List.fold prelims ~init:(-1) ~f:(fun a x ->
                  if x <= succ halflen then succ a else a)
            in
            let indexes =
              List.take prelims (List.length prelims - lows) |> Array.of_list
            in
            loopandprint
              (Array.length indexes * cycles)
              prefix finaltext width suffix lastchar sleep indexes
          else
            [| lenminuswidth |]
            |> loopandprint cycles prefix finaltext width suffix lastchar sleep
        end
      end
  end;

  match terminator with
  | Newline -> ()
  | _ ->
      Externs.unsafe_output_char stdout '\n';
      Externs.unsafe_flush stdout
