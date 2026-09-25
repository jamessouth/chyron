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
    (* print_endline str; *)
    let bytelen, _ =
      Uuseg_string.fold_utf_8 `Grapheme_cluster
        (fun (bytecount, charcount) char ->
          if charcount >= visualchars then (bytecount, charcount)
          else (bytecount + String.length char, succ charcount))
        (0, 0) str
    in
    (* print_endline @@ string_of_int bytelen; *)
    bytelen
  in

  let bytesofcolumns str cols =
    (* print_endline str; *)
    let bytelen, _ =
      Uuseg_string.fold_utf_8 `Grapheme_cluster
        (fun (bytecount, col) char ->
          if col > cols then (-1, col)
          else if col = cols then (bytecount, col)
          else
            ( bytecount + String.length char,
              let a =
                String.Utf8.get
                  (String.Utf8.of_string_unchecked char)
                  ~byte_pos:0
              in
              let b = Uucp.Break.east_asian_width a in
              let c = match b with `W | `F -> 2 | _ -> 1 in
              col + c ))
        (0, 0) str
    in
    (* print_endline @@ string_of_int bytelen; *)
    bytelen
  in
  let ucinds rev cl sptfn accfn wid =
    let open Universal.Visual in
    match visual with
    | Chars ->
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
    | Columns ->
        let rec loop pos acc = function
          | [] -> if rev then List.rev acc else acc
          | h :: t ->
              let lt = h :: t in
              let str = String.concat lt in
              let bts = bytesofcolumns str wid in
              let l, r = List.split_n lt (sptfn lt) in
              if bts < 0 then
                (loop [@tailcall]) (String.length (String.concat l) + pos) acc r
              else
                (loop [@tailcall])
                  (String.length (String.concat l) + pos)
                  (accfn str bts pos acc) r
        in
        loop 0 [] cl
  in
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
    Universal.printandterm
      Universal.{ prefix; suffix; terminator; visual; width }
  in
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
    loop (pwlen * cycles) 0
  in
  let lenminuswidth =
    let open Universal.Visual in
    match visual with
    | Chars -> totallen - bytesofutfchars (String.concat charlist) width
    | Columns -> totallen - bytesofcolumns (String.concat charlist) width
  in
  let brword =
   fun rev ->
    print_endline "rw ";
    ucinds rev charlist wordsplitfn
      (fun str bts _ acc -> (String.length str - bts, bts) :: acc)
      width
  in
  let blchar =
   fun rev ->
    print_endline "lc ";
    ucinds rev revcharlist (fun _ -> 1) accmfn width
  in

  let blword =
   fun rev ->
    print_endline "lw ";
    ucinds rev revcharlist wordsplitfn accmfn width
  in
  let rchar =
   fun rev ->
    print_endline "rc ";
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
