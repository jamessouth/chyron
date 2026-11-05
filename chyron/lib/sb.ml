open Core

module Step = struct
  type t = Char | Word [@@deriving enumerate, sexp]
end

type sb_values = {
  joined_bytes : bytes;
  joined_text_len : int;
  unicode_char_list : string list;
  visual_chars : int;
  visual_columns : int;
}

let step_arg =
  Command.Arg_type.enumerated_sexpable ~accept_unique_prefixes:true
    ~case_sensitive:false ~list_values_in_help:true
    (module Step : Command.Enumerable_sexpable with type t = Step.t)

let bytesofutfchars str visualchars =
  let bytelen, _ =
    Uuseg_string.fold_utf_8 `Grapheme_cluster
      (fun (bytecount, charcount) char ->
        if charcount >= visualchars then (bytecount, charcount)
        else (bytecount + String.length char, succ charcount))
      (0, 0) str
  in
  bytelen

let sbvals text =
  let joined_text = String.concat ~sep:" " text in
  let gclist = Universal.uc_charlist joined_text in
  {
    joined_bytes = Bytes.of_string joined_text;
    joined_text_len = String.length joined_text;
    unicode_char_list = gclist;
    visual_chars = List.length gclist;
    visual_columns =
      List.fold gclist ~init:0 ~f:(fun acc x -> acc + Universal.eaw x);
  }

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

let wordsplitfn chr =
  chr
  |> List.take_while ~f:(fun s -> String.( <> ) s " ")
  |> List.length |> succ

let accmfn _ bts pos acc = (pos, bts) :: acc
let takeappend r l = List.take r 1 |> List.append l

let loopandprint ltfunc print term ft cycles pwlist =
  let pwslist = ltfunc pwlist in
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
    if ticks <= 0 then term ()
    else begin
      print ft
        (Array.unsafe_get indexes idx)
        (Array.unsafe_get indexes (succ idx));
      Universal.Externs.caml_clock_nanosleep
        (Array.unsafe_get indexes (idx |> succ |> succ));
      let nidx = if idx = arrlen then 0 else idx + jumpdist in
      (loop [@tailcall]) (pred ticks) nidx
    end
  in
  loop (List.length pwlist * cycles) 0

(* let charlist ft = Universal.uc_charlist (Bytes.to_string ft) *)

(* let sbfuncs ft width =
  let brword =
   fun rev ->
    ucinds rev charlist wordsplitfn
      (fun str bts _ acc -> (String.length str - bts, bts) :: acc)
      width
  in
  let blchar = fun rev -> ucinds rev revcharlist (fun _ -> 1) accmfn width in
  let blword = fun rev -> ucinds rev revcharlist wordsplitfn accmfn width in
  let rchar = fun rev -> ucinds rev revcharlist (fun _ -> 1) accmfn width in
  (lenminuswidth, totallen asr 1,    totallen) *)

let doublewidth mainlist extended linefunc wid cycles print term buflen
    splitfunc mcount =
  let buffer = Bytes.create buflen in
  let getletters (acc_chars, acc_cols) (nchar, nwidth, _) =
    let open Continue_or_stop in
    if acc_cols < wid then
      let tot = nwidth + acc_cols in
      if tot > wid then Stop (" " :: acc_chars, succ acc_cols)
      else Continue (nchar :: acc_chars, nwidth + acc_cols)
    else Stop (acc_chars, acc_cols)
  in
  let rec loop ticks lt mc =
    match (ticks < 1, lt) with
    | true, _ -> term ()
    | false, [] -> loop (pred ticks) mainlist mcount
    | false, (hd_char, hd_width, hd_sleep) :: t ->
        let letters, _ =
          List.fold_until (List.append lt extended) ~init:([], 0) ~f:getletters
            ~finish:Fn.id
        in
        let line = String.concat @@ linefunc letters in
        let llen = String.length line in
        Bytes.From_string.unsafe_blit ~src:line ~src_pos:0 ~dst:buffer
          ~dst_pos:0 ~len:llen;
        print buffer 0 llen;
        Universal.Externs.caml_clock_nanosleep hd_sleep;
        let nlist, dec = splitfunc hd_char hd_width hd_sleep mc t in
        (loop [@tailcall]) ticks nlist (mc - dec)
  in
  loop cycles mainlist mcount
