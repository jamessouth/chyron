open Core

module Step = struct
  type t = Char | Word [@@deriving enumerate, sexp]
end

let step_arg =
  Command.Arg_type.enumerated_sexpable ~accept_unique_prefixes:true
    ~case_sensitive:false ~list_values_in_help:true
    (module Step : Command.Enumerable_sexpable with type t = Step.t)

let sbvals text =
  let joined_text = String.concat ~sep:" " text in
  ( Bytes.of_string joined_text,
    String.length joined_text,
    List.length (Universal.uc_charlist joined_text) )

let sbfuncs ltfunc ft Universal.{ prefix; suffix; terminator; visual; width }
    cycles =
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
  print_endline @@ Bytes.to_string ft;
  let charlist = Universal.uc_charlist (Bytes.to_string ft) in
  let revcharlist = List.rev charlist in
  let totallen = Bytes.length ft in
  let wordsplitfn chr =
    chr
    |> List.take_while ~f:(fun s -> String.( <> ) s " ")
    |> List.length |> succ
  in
  let accmfn _ bts pos acc = (pos, bts) :: acc in
  let takeappend r l = List.take r 1 |> List.append l in
  let print, term =
    let open Universal in
    printandterm { prefix; suffix; terminator; visual; width }
  in
  let loopandprint pwlist =
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
  in
  let lenminuswidth =
    totallen - bytesofutfchars (String.concat charlist) width
  in
  let brword =
   fun rev ->
    print_string "rw ";
    ucinds rev charlist wordsplitfn
      (fun str bts _ acc -> (String.length str - bts, bts) :: acc)
      width
  in
  let blchar =
   fun rev ->
    print_string "lc ";
    ucinds rev revcharlist (fun _ -> 1) accmfn width
  in
  let blword =
   fun rev ->
    print_string "lw ";
    ucinds rev revcharlist wordsplitfn accmfn width
  in
  let rchar =
   fun rev ->
    print_string "rc ";
    ucinds rev revcharlist (fun _ -> 1) accmfn width
  in
  ( loopandprint,
    lenminuswidth,
    totallen asr 1,
    brword,
    blchar,
    blword,
    rchar,
    takeappend,
    totallen )

let doublewidth chl rev wid cycles print term buflen =
  let buffer = Bytes.create buflen in

  let rec loop ticks cols acc lt =
    match (ticks < 1, lt) with
    | true, _ -> term ()
    | false, [] -> loop (pred ticks) 0 [] chl
    | false, (first_h, second_h, third_h) :: t ->
        Printf.printf "%d %s -- " cols
          (List.to_string
             ~f:(fun (a, b, c) ->
               a ^ "_" ^ string_of_int b ^ "_" ^ string_of_int c)
             acc);
        if cols < wid then
          loop ticks (cols + second_h) ((first_h, second_h, third_h) :: acc) t
        else if cols > wid then (
          (* print_endline "greater"; *)
          let hd, tl = List.split_n acc 1 in
          let _, _, third_hd = List.hd_exn (List.rev tl) in
          let line =
            (if rev then
               List.map
                 (List.rev ((" ", 1, third_hd) :: tl))
                 ~f:(fun (a, _, _) -> a)
             else List.map ((" ", 1, third_hd) :: tl) ~f:(fun (a, _, _) -> a))
            |> String.concat
          in

          let llen = String.length line in
          Bytes.From_string.unsafe_blit ~src:line ~src_pos:0 ~dst:buffer
            ~dst_pos:0 ~len:llen;
          print buffer 0 llen;
          Universal.Externs.caml_clock_nanosleep third_hd;

          let last = List.last_exn tl in
          let nacc = List.drop_last_exn tl in
          let _, second_last, _ = last in
          let facc =
            if second_last = 2 then
              List.hd_exn hd :: List.rev ((" ", 1, third_h) :: List.rev nacc)
            else List.hd_exn hd :: nacc
          in
          let ncols =
            List.fold facc ~init:0 ~f:(fun acm (_, b, _) -> acm + b)
          in
          loop ticks ncols facc lt)
        else (* print_endline "equal"; *)
          let line =
            (if rev then List.map (List.rev acc) ~f:(fun (a, _, _) -> a)
             else List.map acc ~f:(fun (a, _, _) -> a))
            |> String.concat
          in
          let _, _, hd_sleep =
            if rev then List.hd_exn (List.rev acc)
            else List.hd_exn (List.rev acc)
          in
          (* print_endline @@ string_of_int hd_sleep; *)
          let llen = String.length line in
          Bytes.From_string.unsafe_blit ~src:line ~src_pos:0 ~dst:buffer
            ~dst_pos:0 ~len:llen;
          print buffer 0 llen;
          Universal.Externs.caml_clock_nanosleep hd_sleep;

          let last = List.last_exn acc in
          let nacc = List.drop_last_exn acc in
          let _, second_last, _ = last in
          let facc =
            if second_last = 2 then
              (first_h, second_h, third_h)
              :: List.rev ((" ", 1, third_h) :: List.rev nacc)
            else (first_h, second_h, third_h) :: nacc
          in
          let ncols =
            List.fold facc ~init:0 ~f:(fun acm (_, b, _) -> acm + b)
          in
          loop ticks ncols facc t
  in
  loop cycles 0 [] chl
