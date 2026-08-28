open Core
open Chyron
(* open Core_bench *)

let uflags : universalflags Command.Param.t =
  let%map_open.Command cycles =
    flag_optional_with_default_doc "--cycles" ~aliases:[ "-c" ] Ints.zeroplus
      (fun x -> Int.sexp_of_t x)
      ~default:65_536 ~doc:"int number of scroll cycles of TEXT\n"
  and prefix =
    flag_optional_with_default_doc "--prefix" ~aliases:[ "-p" ] string
      (fun x -> String.sexp_of_t x)
      ~default:"" ~doc:"string prefix TEXT at left of display\n"
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
  { cycles; prefix; sleep; suffix; terminator; width }

let bflags : bounceflags Command.Param.t =
  let%map_open.Command endcap_char =
    flag_optional_with_default_doc "--endcap-char" ~aliases:[ "-ec" ] char
      (fun x -> Char.sexp_of_t x)
      ~default:' ' ~doc:"char endcap between end and start of TEXT\n"
  and endcap_len =
    flag_optional_with_default_doc "--endcap-len" ~aliases:[ "-el" ]
      Ints.oneplus
      (fun x -> Int.sexp_of_t x)
      ~default:1 ~doc:"int minimum length of endcap\n"
  and rest =
    flag_optional_with_default_doc "--rest" ~aliases:[ "-r" ] Ints.zeroplus
      (fun x -> Int.sexp_of_t x)
      ~default:0 ~doc:"int additional sleep in ms for frames at extremes\n"
  and scroll_unit =
    flag_optional_with_default_doc "--scroll-unit" ~aliases:[ "-sc" ]
      Scroll_unit.arg Scroll_unit.sexp_of_t ~default:Char
      ~doc:"string scroll TEXT by character or by word\n"
  in
  { endcap_char; endcap_len; rest; scroll_unit }

let sflags : scrollflags Command.Param.t =
  let%map_open.Command direction =
    flag_optional_with_default_doc "--direction" ~aliases:[ "-d" ] Direction.arg
      Direction.sexp_of_t ~default:Left
      ~doc:"string scroll TEXT left, right, or bounce\n"
  and scroll_mode =
    flag_optional_with_default_doc "--scroll-mode" ~aliases:[ "-m" ]
      Scroll_mode.arg Scroll_mode.sexp_of_t ~default:Wrap
      ~doc:"string reset TEXT, wrap around, or split-flap\n"
  in
  { direction; scroll_mode }

let scroll =
  Command.basic ~summary:"scroll mode summary"
    ~readme:(fun () -> "scroll mode details")
    (let%map_open.Command text =
       anon (non_empty_sequence_as_list ("text" %: string))
     and uflags
     and sflags
     and bflags in
     fun () -> run_scroll text uflags sflags bflags)

let bounce =
  Command.basic ~summary:"bounce mode summary"
    ~readme:(fun () -> "bounce mode details")
    (let%map_open.Command text =
       anon (non_empty_sequence_as_list ("text" %: string))
     and uflags
     and bflags in
     fun () -> run_bounce text uflags bflags)

let () =
  Command_unix.run ~version:"1.0" ~build_info:"tbd"
    (Command.group ~summary:"group summary"
       [ ("bounce", bounce); ("scroll", scroll) ])
