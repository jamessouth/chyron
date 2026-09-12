open Chyron
open Core
(* open Core_bench *)

module Ints = struct
  let parseint ~min num =
    match num |> int_of_string_opt with
    | Some n -> Int.max min n
    | None -> invalid_arg "not an int"

  let zeroplus = Command.Arg_type.create (parseint ~min:0)
  let oneplus = Command.Arg_type.create (parseint ~min:1)
  let twoplus = Command.Arg_type.create (parseint ~min:2)
end

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
      terminator_arg sexp_of_terminator ~default:Newline
      ~doc:"string print TEXT with newline, return, or space\n"
  and width =
    flag_optional_with_default_doc "--width" ~aliases:[ "-w" ] Ints.twoplus
      (fun x -> Int.sexp_of_t x)
      ~default:15 ~doc:"int display width of TEXT, exclusive of {pre,suf}fix\n"
  in
  { prefix; suffix; terminator; width }

let bflags =
  let open Bounce in
  let%map_open.Command cycles =
    flag_optional_with_default_doc "--cycles" ~aliases:[ "-c" ] Ints.oneplus
      (fun x -> Int.sexp_of_t x)
      ~default:65_536 ~doc:"int number of complete bounce cycles of TEXT\n"
  and endcap_char =
    flag_optional_with_default_doc "--endcap-char" ~aliases:[ "-e" ] char
      (fun x -> Char.sexp_of_t x)
      ~default:' ' ~doc:"char endcap on ends of TEXT\n"
  and rest =
    flag_optional_with_default_doc "--rest" ~aliases:[ "-r" ] Ints.zeroplus
      (fun x -> Int.sexp_of_t x)
      ~default:0 ~doc:"int additional sleep in ms for frames at extremes\n"
  and sleep =
    flag_optional_with_default_doc "--sleep" ~aliases:[ "-s" ] Ints.oneplus
      (fun x -> Int.sexp_of_t x)
      ~default:300 ~doc:"int sleep in ms per step of TEXT\n"
  and step =
    flag_optional_with_default_doc "--step" ~aliases:[ "-o" ] Sb.Step.arg
      Sb.Step.sexp_of_t ~default:Sb.Step.Char
      ~doc:"string step TEXT by character or by word\n"
  in
  { cycles; endcap_char; rest; step; sleep }

let scflags =
  let open Scroll in
  let%map_open.Command cycles =
    flag_optional_with_default_doc "--cycles" ~aliases:[ "-c" ] Ints.oneplus
      (fun x -> Int.sexp_of_t x)
      ~default:65_536 ~doc:"int number of scroll cycles of TEXT\n"
  and direction =
    flag_optional_with_default_doc "--direction" ~aliases:[ "-d" ] direction_arg
      sexp_of_direction ~default:Left
      ~doc:"string scroll TEXT to left or right\n"
  and endcap_char =
    flag_optional_with_default_doc "--endcap-char" ~aliases:[ "-e" ] char
      (fun x -> Char.sexp_of_t x)
      ~default:' ' ~doc:"char endcap between end and start of TEXT\n"
  and endcap_len =
    flag_optional_with_default_doc "--endcap-len" ~aliases:[ "-l" ] Ints.oneplus
      (fun x -> Int.sexp_of_t x)
      ~default:1 ~doc:"int minimum length of endcap\n"
  and rest =
    flag_optional_with_default_doc "--rest" ~aliases:[ "-r" ] Ints.zeroplus
      (fun x -> Int.sexp_of_t x)
      ~default:0 ~doc:"int additional sleep in ms for frames at extremes\n"
  and step =
    flag_optional_with_default_doc "--step" ~aliases:[ "-o" ] Sb.Step.arg
      Sb.Step.sexp_of_t ~default:Sb.Step.Char
      ~doc:"string scroll TEXT by character or by word\n"
  and mode =
    flag_optional_with_default_doc "--mode" ~aliases:[ "-m" ] mode_arg
      sexp_of_mode ~default:Wrap
      ~doc:"string wrap TEXT around to other side or reset to start\n"
  and sleep =
    flag_optional_with_default_doc "--sleep" ~aliases:[ "-s" ] Ints.oneplus
      (fun x -> Int.sexp_of_t x)
      ~default:300 ~doc:"int sleep in ms per scroll of TEXT\n"
  in
  { cycles; direction; endcap_char; endcap_len; rest; mode; step; sleep }

let sfflags =
  let open Split_flap in
  let%map_open.Command sfcycles =
    flag_optional_with_default_doc "--cycles" ~aliases:[ "-c" ] Ints.oneplus
      (fun x -> Int.sexp_of_t x)
      ~default:65_536 ~doc:"int number of cycles through the lines of TEXT\n"
  and flip_hi_bound =
    flag_optional_with_default_doc "--flip-hi-bound" ~aliases:[ "-h" ]
      Ints.twoplus
      (fun x -> Int.sexp_of_t x)
      ~default:50 ~doc:"int char flips high bound per character position\n"
  and flip_lo_bound =
    flag_optional_with_default_doc "--flip-lo-bound" ~aliases:[ "-l" ]
      Ints.zeroplus
      (fun x -> Int.sexp_of_t x)
      ~default:20 ~doc:"int char flips low bound per character position\n"
  and flip_sleep =
    flag_optional_with_default_doc "--flip-sleep" ~aliases:[ "-f" ] Ints.oneplus
      (fun x -> Int.sexp_of_t x)
      ~default:60 ~doc:"int sleep in ms per char flip\n"
  and justify =
    flag_optional_with_default_doc "--justify" ~aliases:[ "-j" ] justify_arg
      sexp_of_justify ~default:Center
      ~doc:"string align TEXT to left, right, or center\n"
  and sfsleep =
    flag_optional_with_default_doc "--sleep" ~aliases:[ "-s" ] Ints.oneplus
      (fun x -> Int.sexp_of_t x)
      ~default:1500 ~doc:"int sleep in ms per line after char flips complete\n"
  in
  if flip_hi_bound < flip_lo_bound then
    invalid_arg "flip_hi_bound is less than flip_lo_bound";
  { sfcycles; flip_hi_bound; flip_lo_bound; flip_sleep; justify; sfsleep }

let bounce =
  Command.basic ~summary:"Bounce TEXT left and right."
    ~readme:(fun () ->
      "Bounce TEXT back and forth by --step every --sleep ms --cycles times. \
       If TEXT\n\
       is shorter than --width, an endcap string made of --endcap-char will be \
       added\n\
       to each end. Each frame of TEXT prints --width characters plus any \
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
      "Scroll TEXT by --step in --mode every --sleep ms --cycles times in \
       --direction.\n\
       An endcap string made of --endcap-char with length --endcap-len will be \
       added\n\
       between the end and beginning of TEXT. Each frame of TEXT prints --width\n\
       characters plus any --prefix and --suffix, plus --terminator. An optional\n\
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
      "Break TEXT into lines and show --cycles times for --sleep ms per line. \
       Each\n\
       character flips between --flip-lo-bound and --flip-hi-bound times at a \
       rate\n\
       of --flip-sleep ms per flip. Each line is --justify aligned and prints\n\
       --width characters plus any --prefix and --suffix, plus --terminator.")
    (let%map_open.Command text =
       anon (non_empty_sequence_as_list ("text" %: string))
     and uflags
     and sfflags in
     fun () -> Split_flap.run_split_flap text uflags sfflags)

let () =
  Command_unix.run ~version:"1.0" ~build_info:"tbd"
    (Command.group ~summary:"Bounce, scroll, or split-flap TEXT"
       [ ("bounce", bounce); ("scroll", scroll); ("split-flap", split_flap) ])
