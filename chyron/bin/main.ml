open Core
open Chyron
(* open Core_bench *)

let flags : cliflags Command.Param.t =
  let%map_open.Command cycles =
    flag_optional_with_default_doc "--cycles" ~aliases:[ "-c" ] Ints.zeroplus
      (fun x -> Int.sexp_of_t x)
      ~default:65_536 ~doc:"int number of scroll cycles of TEXT\n"
  and direction =
    flag_optional_with_default_doc "--direction" ~aliases:[ "-d" ] Direction.arg
      Direction.sexp_of_t ~default:Left
      ~doc:"string scroll TEXT left, right, or bounce\n"
  and endcap_char =
    flag_optional_with_default_doc "--endcap-char" ~aliases:[ "-ec" ] char
      (fun x -> Char.sexp_of_t x)
      ~default:' ' ~doc:"char endcap between end and start of TEXT\n"
  and endcap_len =
    flag_optional_with_default_doc "--endcap-len" ~aliases:[ "-el" ]
      Ints.oneplus
      (fun x -> Int.sexp_of_t x)
      ~default:1 ~doc:"int minimum length of endcap\n"
  and mode =
    flag_optional_with_default_doc "--mode" ~aliases:[ "-m" ] Mode.arg
      Mode.sexp_of_t ~default:Wrap
      ~doc:"string reset TEXT, wrap around, or split-flap\n"
  and prefix =
    flag_optional_with_default_doc "--prefix" ~aliases:[ "-p" ] string
      (fun x -> String.sexp_of_t x)
      ~default:"" ~doc:"string prefix TEXT at left of display\n"
  and rest =
    flag_optional_with_default_doc "--rest" ~aliases:[ "-r" ] Ints.zeroplus
      (fun x -> Int.sexp_of_t x)
      ~default:0 ~doc:"int additional sleep in ms for frames at extremes\n"
  and scroll =
    flag_optional_with_default_doc "--scroll" ~aliases:[ "-sc" ] Scroll.arg
      Scroll.sexp_of_t ~default:Char
      ~doc:"string scroll TEXT by character or by word\n"
  and sleep =
    flag_optional_with_default_doc "--sleep" ~aliases:[ "-sl" ] Ints.oneplus
      (fun x -> Int.sexp_of_t x)
      ~default:300 ~doc:"int sleep in ms per scroll of TEXT\n"
  and suffix =
    flag_optional_with_default_doc "--suffix" ~aliases:[ "-su" ] string
      (fun x -> String.sexp_of_t x)
      ~default:"" ~doc:"string suffix TEXT at right of display\n"
  and terminator =
    flag_optional_with_default_doc "--terminator" ~aliases:[ "-t" ]
      Terminator.arg Terminator.sexp_of_t ~default:Newline
      ~doc:"string print TEXT with newline, return, or space\n"
  and width =
    flag_optional_with_default_doc "--width" ~aliases:[ "-w" ] Ints.twoplus
      (fun x -> Int.sexp_of_t x)
      ~default:15 ~doc:"int display width of TEXT, exclusive of {pre,suf}fix\n"
  in
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
  }

let () =
  let summ = "write me" in
  let mdi = "write me with more details" in
  Command_unix.run ~version:"1.0" ~build_info:"tbd"
    (Command.basic ~summary:summ
       ~readme:(fun () -> mdi)
       (let%map_open.Command text =
          anon (non_empty_sequence_as_list ("text" %: string))
        and flags in
        fun () -> run text flags))
