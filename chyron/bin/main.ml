open Chyron
open Core
(* open Core_bench *)

let cycmin = 65_536

let univ_flags =
  let open Universal in
  let%map_open.Command prefix =
    flag_optional_with_default_doc "--prefix" ~aliases:[ "-p" ] string
      String.sexp_of_t ~default:""
      ~doc:"string prefix TEXT at left of display\n"
  and suffix =
    flag_optional_with_default_doc "--suffix" ~aliases:[ "-x" ] string
      String.sexp_of_t ~default:""
      ~doc:"string suffix TEXT at right of display\n"
  and terminator =
    flag_optional_with_default_doc "--terminator" ~aliases:[ "-t" ]
      terminator_arg Terminator.sexp_of_t ~default:Terminator.LF
      ~doc:"string terminating character when printing TEXT\n"
  and width =
    flag_optional_with_default_doc "--width" ~aliases:[ "-w" ] int Int.sexp_of_t
      ~default:17 ~doc:"int display width of TEXT, exclusive of {pre,suf}fix\n"
  in
  if width < 2 then invalid_arg "width less than 2";
  { prefix; suffix; terminator; width }

let bounce_flags =
  let open Bounce in
  let%map_open.Command cycles =
    flag_optional_with_default_doc "--cycles" ~aliases:[ "-c" ] int
      Int.sexp_of_t ~default:cycmin
      ~doc:"int number of complete bounce cycles of TEXT\n"
  and endcap_char =
    flag_optional_with_default_doc "--endcap-char" ~aliases:[ "-e" ] char
      Char.sexp_of_t ~default:' ' ~doc:"char endcap on ends of TEXT\n"
  and rest =
    flag_optional_with_default_doc "--rest" ~aliases:[ "-r" ] int Int.sexp_of_t
      ~default:0 ~doc:"int additional sleep in ms for frames at extremes\n"
  and sleep =
    flag_optional_with_default_doc "--sleep" ~aliases:[ "-s" ] int Int.sexp_of_t
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

let scroll_flags =
  let open Scroll in
  let%map_open.Command cycles =
    flag_optional_with_default_doc "--cycles" ~aliases:[ "-c" ] int
      Int.sexp_of_t ~default:cycmin ~doc:"int number of scroll cycles of TEXT\n"
  and direction =
    flag_optional_with_default_doc "--direction" ~aliases:[ "-d" ] direction_arg
      Direction.sexp_of_t ~default:Direction.Left
      ~doc:"string direction to scroll TEXT\n"
  and endcap_char =
    flag_optional_with_default_doc "--endcap-char" ~aliases:[ "-e" ] char
      Char.sexp_of_t ~default:' '
      ~doc:"char endcap between end and start of TEXT\n"
  and endcap_len =
    flag_optional_with_default_doc "--endcap-len" ~aliases:[ "-l" ] int
      Int.sexp_of_t ~default:1 ~doc:"int minimum length of endcap\n"
  and mode =
    flag_optional_with_default_doc "--mode" ~aliases:[ "-m" ] mode_arg
      Mode.sexp_of_t ~default:Mode.Wrap
      ~doc:"string wrap TEXT around to other side or reset to start\n"
  and rest =
    flag_optional_with_default_doc "--rest" ~aliases:[ "-r" ] int Int.sexp_of_t
      ~default:0 ~doc:"int additional sleep in ms for frames at extremes\n"
  and sleep =
    flag_optional_with_default_doc "--sleep" ~aliases:[ "-s" ] int Int.sexp_of_t
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

let univ_sf_flags =
  let open Split_flap in
  let%map_open.Command charsets =
    flag_optional_with_default_doc "--charsets" ~aliases:[ "-a" ] charset_arg
      (fun x -> List.sexp_of_t Charset.sexp_of_t x)
      ~default:[ Charset.Uppers; Charset.Symbols1 ]
      ~doc:
        (String.concat
           [
             "string characters to flip through. pass a comma- separated list, \
              either quoted or without spaces, from these names: ";
             List.to_string
               ~f:(fun x -> Charset.sexp_of_t x |> string_of_sexp)
               Charset.all;
             "\n";
           ])
  and custom_chars =
    flag_optional_with_default_doc "--custom-str" ~aliases:[ "-b" ] string
      String.sexp_of_t ~default:""
      ~doc:"string additional characters to flip through\n"
  and cycles =
    flag_optional_with_default_doc "--cycles" ~aliases:[ "-c" ] int
      Int.sexp_of_t ~default:cycmin
      ~doc:"int number of cycles through the lines of TEXT\n"
  and flip_sleep =
    flag_optional_with_default_doc "--flip-sleep" ~aliases:[ "-f" ] int
      Int.sexp_of_t ~default:53 ~doc:"int sleep in ms per char flip\n"
  and justify =
    flag_optional_with_default_doc "--justify" ~aliases:[ "-j" ] justify_arg
      Justify.sexp_of_t ~default:Justify.Center
      ~doc:"string where to align TEXT\n"
  and sleep =
    flag_optional_with_default_doc "--sleep" ~aliases:[ "-s" ] int Int.sexp_of_t
      ~default:1499 ~doc:"int sleep in ms per line after char flips complete\n"
  in
  if cycles < 1 then invalid_arg "cycles less than 1";
  if flip_sleep < 1 then invalid_arg "flip_sleep less than 1";
  if sleep < 1 then invalid_arg "sleep less than 1";
  { charsets; custom_chars; cycles; flip_sleep; justify; sleep }

let alpha_sf_flags =
  let open Split_flap in
  let%map_open.Command direction =
    flag_optional_with_default_doc "--direction" ~aliases:[ "-d" ] direction_arg
      Direction.sexp_of_t ~default:Direction.Up
      ~doc:"string direction to flip through --charsets\n"
  in
  { direction }

let rando_sf_flags =
  let open Split_flap in
  let%map_open.Command flip_hi_bound =
    flag_optional_with_default_doc "--flip-hi-bound" ~aliases:[ "-h" ] int
      Int.sexp_of_t ~default:61
      ~doc:"int char flips high bound per character position\n"
  and flip_lo_bound =
    flag_optional_with_default_doc "--flip-lo-bound" ~aliases:[ "-l" ] int
      Int.sexp_of_t ~default:13
      ~doc:"int char flips low bound per character position\n"
  in
  if flip_hi_bound < 2 then invalid_arg "flip_hi_bound less than 2";
  if flip_lo_bound < 0 then invalid_arg "flip_lo_bound less than 0";
  if flip_hi_bound < flip_lo_bound then
    invalid_arg "flip_hi_bound is less than flip_lo_bound";
  { flip_hi_bound; flip_lo_bound }

let bounce =
  Command.basic ~summary:"bounce TEXT left and right"
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
     and univ_flags
     and bounce_flags in
     fun () -> Bounce.run_bounce text univ_flags bounce_flags)

let scroll =
  Command.basic ~summary:"scroll TEXT left or right"
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
     and univ_flags
     and scroll_flags in
     fun () -> Scroll.run_scroll text univ_flags scroll_flags)

let split_flap_alpha =
  Command.basic ~summary:"flip through characters in alphabetical order"
    ~readme:(fun () ->
      "Break TEXT into lines and show each --cycles times for --sleep ms per \
       line.\n\
       Each character in a line flips orderly through the members of \
       --charsets at\n\
       a rate of --flip-sleep ms per flip until the target character is reached.\n\
       Set . Each line is --justify aligned and\n\
       prints --width characters, plus any --prefix and --suffix, plus \
       --terminator.")
    (let%map_open.Command text =
       anon (non_empty_sequence_as_list ("text" %: string))
     and univ_flags
     and univ_sf_flags
     and alpha_sf_flags in
     fun () ->
       Split_flap.run_split_flap_alpha text univ_flags univ_sf_flags
         alpha_sf_flags)

let split_flap_rando =
  Command.basic ~summary:"flip through characters in random order"
    ~readme:(fun () ->
      "Break TEXT into lines and show each --cycles times for --sleep ms per \
       line.\n\
       Each character in a line flips randomly through the members of --charsets\n\
       between --flip-lo-bound and --flip-hi-bound times at a rate of \
       --flip-sleep\n\
       ms per flip. Each line is --justify aligned and prints --width \
       characters, plus\n\
       any --prefix and --suffix, plus --terminator.")
    (let%map_open.Command text =
       anon (non_empty_sequence_as_list ("text" %: string))
     and univ_flags
     and univ_sf_flags
     and rando_sf_flags in
     fun () ->
       Split_flap.run_split_flap_rando text univ_flags univ_sf_flags
         rando_sf_flags)

let sfgroup =
  Command.group ~summary:"alphabetic or random character flipping"
    [ ("alpha", split_flap_alpha); ("rando", split_flap_rando) ]

let () =
  Command_unix.run ~version:"1.0" ~build_info:"tbd"
    (Command.group ~summary:"bounce, scroll, or split-flap TEXT"
       [ ("bounce", bounce); ("scroll", scroll); ("split-flap", sfgroup) ])
