open Core

module Step = struct
  type t = Char | Word [@@deriving enumerate, sexp]
end

let step_arg =
  Command.Arg_type.enumerated_sexpable ~accept_unique_prefixes:true
    ~case_sensitive:false ~list_values_in_help:true
    (module Step : Command.Enumerable_sexpable with type t = Step.t)

let sbvals text visual =
  let joined_text = String.concat ~sep:" " text in
  ( Bytes.of_string joined_text,
    String.length joined_text,
    let gclist = Universal.uc_charlist joined_text in
    let open Universal.Visual in
    match visual with
    | Chars -> List.length gclist
    | Columns ->
        List.fold gclist ~init:0 ~f:(fun acc x -> acc + Universal.eaw x) )

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
    print_string "rw \n";
    ucinds rev charlist wordsplitfn
      (fun str bts _ acc -> (String.length str - bts, bts) :: acc)
      width
  in
  let blchar =
   fun rev ->
    print_string "lc \n";
    ucinds rev revcharlist (fun _ -> 1) accmfn width
  in
  let blword =
   fun rev ->
    print_string "lw \n";
    ucinds rev revcharlist wordsplitfn accmfn width
  in
  let rchar =
   fun rev ->
    print_string "rc \n";
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

let doublewidth chl chl2 func wid cycles print term buflen func2 =
  let buffer = Bytes.create buflen in

  let rec loop ticks lt =
    (* Printf.printf "lt: %d %s -- \n" ticks
      (List.to_string
         ~f:(fun (a, b, c) -> a ^ "_" ^ string_of_int b ^ "_" ^ string_of_int c)
         lt); *)
    match (ticks < 1, lt) with
    | true, _ -> term ()
    | false, [] -> loop (pred ticks) chl
    | false, (_char, charwth, slp) :: t ->
        (* Printf.printf "head: %s %d %d\n" char charwth slp; *)
        let letters, _ =
          List.fold_until (List.append lt chl2) ~init:([], 0)
            ~f:(fun (chars, cols) (char, wth, _) ->
              let open Continue_or_stop in
              (* Printf.printf " fld: %s %d -- %s  %d\n" char wth
                ((List.to_string ~f:Fn.id) chars)
                cols; *)
              if cols < wid then
                (* let _ = Printf.printf "here " in *)
                let tot = wth + cols in
                if tot > wid then Stop (" " :: chars, succ cols)
                else Continue (char :: chars, wth + cols)
              (* else if cols > wid then
                let _ = Printf.printf "    here2 " in
                (" " :: List.tl_exn chars, cols) *)
                else
                let _ = Printf.printf "        here3 " in
                Stop (chars, cols))
            ~finish:(fun x -> x)
        in
        let line = String.concat @@ func letters in

        let llen = String.length line in
        Bytes.From_string.unsafe_blit ~src:line ~src_pos:0 ~dst:buffer
          ~dst_pos:0 ~len:llen;
        print buffer 0 llen;
        Universal.Externs.caml_clock_nanosleep slp;

        (* let nlist = if charwth = 2 then (" ", 1, slp) :: t else t in *)
        let nlist = func2 charwth slp t in

        loop ticks nlist
  in
  loop cycles chl
