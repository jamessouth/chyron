open Chyron
open Core
(* open Core_bench *)

let cycmin = 65_536

let uflags =
  let open Universal in
  let%map_open.Command prefix =
    flag_optional_with_default_doc "--prefix" ~aliases:[ "-p" ] string
      (fun x -> String.sexp_of_t x)
      ~default:"" ~doc:"string prefix TEXT at left of display\n"
  and suffix =
    flag_optional_with_default_doc "--suffix" ~aliases:[ "-x" ] string
      (fun x -> String.sexp_of_t x)
      ~default:"" ~doc:"string suffix TEXT at right of display\n"
  and terminator =
    flag_optional_with_default_doc "--terminator" ~aliases:[ "-t" ]
      terminator_arg Terminator.sexp_of_t ~default:Terminator.LF
      ~doc:"string terminating character when printing TEXT\n"
  and width =
    flag_optional_with_default_doc "--width" ~aliases:[ "-w" ] int
      (fun x -> Int.sexp_of_t x)
      ~default:17 ~doc:"int display width of TEXT, exclusive of {pre,suf}fix\n"
  in
  if width < 2 then invalid_arg "width less than 2";
  { prefix; suffix; terminator; width }

let bflags =
  let open Bounce in
  let%map_open.Command cycles =
    flag_optional_with_default_doc "--cycles" ~aliases:[ "-c" ] int
      (fun x -> Int.sexp_of_t x)
      ~default:cycmin ~doc:"int number of complete bounce cycles of TEXT\n"
  and endcap_char =
    flag_optional_with_default_doc "--endcap-char" ~aliases:[ "-e" ] char
      (fun x -> Char.sexp_of_t x)
      ~default:' ' ~doc:"char endcap on ends of TEXT\n"
  and rest =
    flag_optional_with_default_doc "--rest" ~aliases:[ "-r" ] int
      (fun x -> Int.sexp_of_t x)
      ~default:0 ~doc:"int additional sleep in ms for frames at extremes\n"
  and sleep =
    flag_optional_with_default_doc "--sleep" ~aliases:[ "-s" ] int
      (fun x -> Int.sexp_of_t x)
      ~default:307 ~doc:"int sleep in ms per step of TEXT\n"
  and step =
    flag_optional_with_default_doc "--step" ~aliases:[ "-o" ] Sb.step_arg
      Sb.Step.sexp_of_t ~default:Sb.Step.Char
      ~doc:"string length that TEXT shifts each frame\n"
  in
  if cycles < 1 then invalid_arg "cycles less than 1";
  if rest < 0 then invalid_arg "rest less than 0";
  if sleep < 1 then invalid_arg "sleep less than 1";
  { cycles; endcap_char; rest; step; sleep }

let scflags =
  let open Scroll in
  let%map_open.Command cycles =
    flag_optional_with_default_doc "--cycles" ~aliases:[ "-c" ] int
      (fun x -> Int.sexp_of_t x)
      ~default:cycmin ~doc:"int number of scroll cycles of TEXT\n"
  and direction =
    flag_optional_with_default_doc "--direction" ~aliases:[ "-d" ] direction_arg
      Direction.sexp_of_t ~default:Direction.Left
      ~doc:"string direction to scroll TEXT\n"
  and endcap_char =
    flag_optional_with_default_doc "--endcap-char" ~aliases:[ "-e" ] char
      (fun x -> Char.sexp_of_t x)
      ~default:' ' ~doc:"char endcap between end and start of TEXT\n"
  and endcap_len =
    flag_optional_with_default_doc "--endcap-len" ~aliases:[ "-l" ] int
      (fun x -> Int.sexp_of_t x)
      ~default:1 ~doc:"int minimum length of endcap\n"
  and mode =
    flag_optional_with_default_doc "--mode" ~aliases:[ "-m" ] mode_arg
      Mode.sexp_of_t ~default:Mode.Wrap
      ~doc:"string wrap TEXT around to other side or reset to start\n"
  and rest =
    flag_optional_with_default_doc "--rest" ~aliases:[ "-r" ] int
      (fun x -> Int.sexp_of_t x)
      ~default:0 ~doc:"int additional sleep in ms for frames at extremes\n"
  and sleep =
    flag_optional_with_default_doc "--sleep" ~aliases:[ "-s" ] int
      (fun x -> Int.sexp_of_t x)
      ~default:311 ~doc:"int sleep in ms per scroll of TEXT\n"
  and step =
    flag_optional_with_default_doc "--step" ~aliases:[ "-o" ] Sb.step_arg
      Sb.Step.sexp_of_t ~default:Sb.Step.Char
      ~doc:"string length that TEXT shifts each frame\n"
  in
  if cycles < 1 then invalid_arg "cycles less than 1";
  if endcap_len < 1 then invalid_arg "endcap_len less than 1";
  if rest < 0 then invalid_arg "rest less than 0";
  if sleep < 1 then invalid_arg "sleep less than 1";
  { cycles; direction; endcap_char; endcap_len; mode; rest; sleep; step }

let sfflags =
  let open Split_flap in
  let%map_open.Command charsets =
    flag_optional_with_default_doc "--charsets" ~aliases:[ "-a" ] charset_arg
      (fun x -> List.sexp_of_t Charset.sexp_of_t x)
      ~default:Charset.all
      ~doc:
        "string characters to flip through. pass a comma- separated list, \
         either quoted or without spaces\n"
  and cycles =
    flag_optional_with_default_doc "--cycles" ~aliases:[ "-c" ] int
      (fun x -> Int.sexp_of_t x)
      ~default:cycmin ~doc:"int number of cycles through the lines of TEXT\n"
  and flip_hi_bound =
    flag_optional_with_default_doc "--flip-hi-bound" ~aliases:[ "-h" ] int
      (fun x -> Int.sexp_of_t x)
      ~default:61 ~doc:"int char flips high bound per character position\n"
  and flip_lo_bound =
    flag_optional_with_default_doc "--flip-lo-bound" ~aliases:[ "-l" ] int
      (fun x -> Int.sexp_of_t x)
      ~default:13 ~doc:"int char flips low bound per character position\n"
  and flip_sleep =
    flag_optional_with_default_doc "--flip-sleep" ~aliases:[ "-f" ] int
      (fun x -> Int.sexp_of_t x)
      ~default:53 ~doc:"int sleep in ms per char flip\n"
  and justify =
    flag_optional_with_default_doc "--justify" ~aliases:[ "-j" ] justify_arg
      Justify.sexp_of_t ~default:Justify.Center
      ~doc:"string where to align TEXT\n"
  and sleep =
    flag_optional_with_default_doc "--sleep" ~aliases:[ "-s" ] int
      (fun x -> Int.sexp_of_t x)
      ~default:1499 ~doc:"int sleep in ms per line after char flips complete\n"
  in
  if cycles < 1 then invalid_arg "cycles less than 1";
  if flip_hi_bound < 2 then invalid_arg "flip_hi_bound less than 2";
  if flip_lo_bound < 0 then invalid_arg "flip_lo_bound less than 0";
  if flip_hi_bound < flip_lo_bound then
    invalid_arg "flip_hi_bound is less than flip_lo_bound";
  if flip_sleep < 1 then invalid_arg "flip_sleep less than 1";
  if sleep < 1 then invalid_arg "sleep less than 1";
  { charsets; cycles; flip_hi_bound; flip_lo_bound; flip_sleep; justify; sleep }

let bounce =
  Command.basic ~summary:"Bounce TEXT left and right."
    ~readme:(fun () ->
      "Bounce TEXT back and forth by --step every --sleep ms --cycles times. \
       If TEXT\n\
       is shorter than --width, an endcap string made of --endcap-char will be \
       added\n\
       to each end. Each frame of TEXT prints --width characters, plus any \
       --prefix\n\
       and --suffix, plus --terminator. An optional --rest can be given to \
       extend the\n\
       on-screen time of some frames that may otherwise only be shown very \
       briefly.")
    (let%map_open.Command text =
       anon (non_empty_sequence_as_list ("text" %: string))
     and uflags
     and bflags in
     fun () -> Bounce.run_bounce text uflags bflags)

let scroll =
  Command.basic ~summary:"Scroll TEXT left or right."
    ~readme:(fun () ->
      "Scroll TEXT by --step in --direction and --mode every --sleep ms \
       --cycles times.\n\
       An endcap string made of --endcap-char with length --endcap-len will be \
       added\n\
       between the end and beginning of TEXT. Each frame of TEXT prints --width\n\
       characters, plus any --prefix and --suffix, plus --terminator. An \
       optional\n\
       --rest can be given to extend the on-screen time of some frames that may\n\
       otherwise only be shown very briefly.")
    (let%map_open.Command text =
       anon (non_empty_sequence_as_list ("text" %: string))
     and uflags
     and scflags in
     fun () -> Scroll.run_scroll text uflags scflags)

let split_flap =
  Command.basic ~summary:"Show TEXT as a split-flap display."
    ~readme:(fun () ->
      "Break TEXT into lines and show each --cycles times for --sleep ms per \
       line.\n\
       Each character flips through the members of --charsets between \
       --flip-lo-bound\n\
       and --flip-hi-bound times at a rate of --flip-sleep ms per flip. Each \
       line\n\
       is --justify aligned and prints --width characters, plus any --prefix and\n\
       --suffix, plus --terminator.")
    (let%map_open.Command text =
       anon (non_empty_sequence_as_list ("text" %: string))
     and uflags
     and sfflags in
     fun () -> Split_flap.run_split_flap text uflags sfflags)

let () =
  Command_unix.run ~version:"1.0" ~build_info:"tbd"
    (Command.group ~summary:"Bounce, scroll, or split-flap TEXT"
       [ ("bounce", bounce); ("scroll", scroll); ("split-flap", split_flap) ])
