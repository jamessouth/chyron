open Chyron
(* open Core_bench *)

let uflags =
  let%map_open.Command prefix =
    flag_optional_with_default_doc "--prefix" ~aliases:[ "-p" ] string
      (fun x -> Core.String.sexp_of_t x)
      ~default:"" ~doc:"string prefix TEXT at left of display\n"
  and suffix =
    flag_optional_with_default_doc "--suffix" ~aliases:[ "-x" ] string
      (fun x -> Core.String.sexp_of_t x)
      ~default:"" ~doc:"string suffix TEXT at right of display\n"
  and terminator =
    flag_optional_with_default_doc "--terminator" ~aliases:[ "-t" ]
      Terminator.arg Terminator.sexp_of_t ~default:Terminator.Newline
      ~doc:"string print TEXT with newline, return, or space\n"
  and width =
    flag_optional_with_default_doc "--width" ~aliases:[ "-w" ] Ints.twoplus
      (fun x -> Core.Int.sexp_of_t x)
      ~default:15 ~doc:"int display width of TEXT, exclusive of {pre,suf}fix\n"
  in
  { prefix; suffix; terminator; width }

let bflags =
  let%map_open.Command endcap_char =
    flag_optional_with_default_doc "--endcap-char" ~aliases:[ "-e" ] char
      (fun x -> Core.Char.sexp_of_t x)
      ~default:' ' ~doc:"char endcap between end and start of TEXT\n"
  and endcap_len =
    flag_optional_with_default_doc "--endcap-len" ~aliases:[ "-l" ] Ints.oneplus
      (fun x -> Core.Int.sexp_of_t x)
      ~default:1 ~doc:"int minimum length of endcap\n"
  and rest =
    flag_optional_with_default_doc "--rest" ~aliases:[ "-r" ] Ints.zeroplus
      (fun x -> Core.Int.sexp_of_t x)
      ~default:0 ~doc:"int additional sleep in ms for frames at extremes\n"
  and scroll_step =
    flag_optional_with_default_doc "--scroll-step" ~aliases:[ "-o" ]
      Scroll_step.arg Scroll_step.sexp_of_t ~default:Scroll_step.Char
      ~doc:"string scroll TEXT by character or by word\n"
  in
  { endcap_char; endcap_len; rest; scroll_step }

let scflags =
  let%map_open.Command direction =
    flag_optional_with_default_doc "--direction" ~aliases:[ "-d" ] Direction.arg
      Direction.sexp_of_t ~default:Direction.Left
      ~doc:"string scroll TEXT to left or right\n"
  and scroll_mode =
    flag_optional_with_default_doc "--scroll-mode" ~aliases:[ "-m" ]
      Scroll_mode.arg Scroll_mode.sexp_of_t ~default:Scroll_mode.Wrap
      ~doc:"string wrap TEXT around to other side or reset to start\n"
  in
  { direction; scroll_mode }

let sbflags =
  let%map_open.Command cycles =
    flag_optional_with_default_doc "--cycles" ~aliases:[ "-c" ] Ints.oneplus
      (fun x -> Core.Int.sexp_of_t x)
      ~default:65_536 ~doc:"int number of scroll cycles of TEXT\n"
  and sleep =
    flag_optional_with_default_doc "--sleep" ~aliases:[ "-s" ] Ints.oneplus
      (fun x -> Core.Int.sexp_of_t x)
      ~default:300 ~doc:"int sleep in ms per scroll of TEXT\n"
  in
  { cycles; sleep }

let sfflags =
  let%map_open.Command sfcycles =
    flag_optional_with_default_doc "--cycles" ~aliases:[ "-c" ] Ints.oneplus
      (fun x -> Core.Int.sexp_of_t x)
      ~default:65_536 ~doc:"int number of cycles through TEXT\n"
  and flip_hi_bound =
    flag_optional_with_default_doc "--flip-hi-bound" ~aliases:[ "-h" ]
      Ints.twoplus
      (fun x -> Core.Int.sexp_of_t x)
      ~default:50 ~doc:"int char flips high bound per character position\n"
  and flip_lo_bound =
    flag_optional_with_default_doc "--flip-lo-bound" ~aliases:[ "-l" ]
      Ints.zeroplus
      (fun x -> Core.Int.sexp_of_t x)
      ~default:20 ~doc:"int char flips low bound per character position\n"
  and flip_sleep =
    flag_optional_with_default_doc "--flip-sleep" ~aliases:[ "-f" ] Ints.oneplus
      (fun x -> Core.Int.sexp_of_t x)
      ~default:60 ~doc:"int sleep in ms per char flip\n"
  and justify =
    flag_optional_with_default_doc "--justify" ~aliases:[ "-j" ] Justify.arg
      Justify.sexp_of_t ~default:Justify.Center
      ~doc:"string align TEXT to left, right, or center\n"
  and sfsleep =
    flag_optional_with_default_doc "--sleep" ~aliases:[ "-s" ] Ints.oneplus
      (fun x -> Core.Int.sexp_of_t x)
      ~default:1500 ~doc:"int sleep in ms per line after char flips complete\n"
  in
  if flip_hi_bound < flip_lo_bound then
    invalid_arg "flip_hi_bound is less than flip_lo_bound";
  { sfcycles; flip_hi_bound; flip_lo_bound; flip_sleep; justify; sfsleep }

let bounce =
  Command.basic ~summary:"bounce mode summary"
    ~readme:(fun () -> "bounce mode details")
    (let%map_open.Command text =
       anon (non_empty_sequence_as_list ("text" %: string))
     and uflags
     and sbflags
     and bflags in
     fun () -> run_bounce text uflags sbflags bflags)

let scroll =
  Command.basic ~summary:"Scroll TEXT left or right."
    ~readme:(fun () ->
      "Scroll TEXT by --scroll-step in --scroll-mode with speed --sleep. Scrolls\n\
       through all of TEXT --cycles times in --direction. An endcap string made\n\
       of --endcap-char with length --endcap-len will sit between the end and\n\
       beginning of TEXT. Each frame of TEXT will be printed in --width with any\n\
       provided --prefix and --suffix, then --terminator. An optional --rest\n\
       can be given to extend the on-screen time of some frames that may \
       otherwise\n\
       only be shown very briefly.")
    (let%map_open.Command text =
       anon (non_empty_sequence_as_list ("text" %: string))
     and uflags
     and sbflags
     and scflags
     and bflags in
     fun () -> run_scroll text uflags sbflags scflags bflags)

let split_flap =
  Command.basic ~summary:"split-flap mode summary"
    ~readme:(fun () -> "split-flap mode details")
    (let%map_open.Command text =
       anon (non_empty_sequence_as_list ("text" %: string))
     and uflags
     and sfflags in
     fun () -> run_split_flap text uflags sfflags)

let () =
  Command_unix.run ~version:"1.0" ~build_info:"tbd"
    (Command.group ~summary:"group summary"
       [ ("bounce", bounce); ("scroll", scroll); ("split-flap", split_flap) ])
