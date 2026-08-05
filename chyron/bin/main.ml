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
      Mode.sexp_of_t ~default:Wrap ~doc:"string reset TEXT or wrap around\n"
  and prefix =
    flag_optional_with_default_doc "--prefix" ~aliases:[ "-p" ] string
      (fun x -> String.sexp_of_t x)
      ~default:"" ~doc:"string prefix TEXT at left of display\n"
  and rest =
    flag_optional_with_default_doc "--rest" ~aliases:[ "-r" ] Ints.zeroplus
      (fun x -> Int.sexp_of_t x)
      ~default:0 ~doc:"int alternate sleep in ms for frames at extremes\n"
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
  let summ = "summary" in
  let mdi = "mdi" in
  Command_unix.run ~version:"1.0" ~build_info:"RWO"
    (Command.basic ~summary:summ
       ~readme:(fun () -> mdi)
       (let%map_open.Command text =
          anon (non_empty_sequence_as_list ("text" %: string))
        and flags in
        fun () ->
          if
            List.fold_until text ~init:false
              ~f:(fun a s ->
                if String.exists s ~f:(fun c -> Char.to_int c > 127) then
                  Stop true
                else Continue false)
              ~finish:(fun a -> a)
          then runuc text flags
          else run text flags))

(* let () =
  Gc.print_stat stderr;
  print_endline "---" *)

(* let () =
  let text = [ "mary had" ] in
  let flags =
    {
      cycles = 1;
      direction = Bounce;
      endcap_char = 'W';
      endcap_len = 2;
      rest = 0;
      terminator = Newline;
      prefix = "XX";
      sleep = 32;
      suffix = "UU";
      width = 7;
    }
  in
  Command_unix.run
    (Bench.make_command
       [ Bench.Test.create ~name:"Optimized" (fun () -> run text flags) ]) *)

(* 
┌───────────┬──────────┬─────────┬────────────┐
│ Name      │ Time/Run │ mWd/Run │ Percentage │
├───────────┼──────────┼─────────┼────────────┤
│ Optimized │  96.61ms │  23.00w │    100.00% │
└───────────┴──────────┴─────────┴────────────┘ *)
(* 
let () =
  let s = Gc.quick_stat () in
  Printf.printf
    "minor_collections=%d major_collections=%d promoted_words=%f \
     minor_words=%f major_words=%f\n"
    s.minor_collections s.major_collections s.promoted_words s.minor_words
    s.major_words

    minor_collections=3 major_collections=1 promoted_words=140615.000000 minor_words=255852.000000 major_words=149839.000000 *)

(* perf record --call-graph dwarf --period=1000 --event=cycles:u *)

(* Performance counter stats for './_build/default/bin/main.exe mary had a little lamb -c 10 -w 17 -e 150' (10 runs):

                 0      context-switches:u               #      0.0 cs/sec  cs_per_second     
                 0      cpu-migrations:u                 #      0.0 migrations/sec  migrations_per_second
             2,042      page-faults:u                    #  51681.0 faults/sec  page_faults_per_second  ( +-  0.02% )
             39.51 msec task-clock:u                     #      0.0 CPUs  CPUs_utilized         ( +-  0.78% )
           123,283      L1-dcache-load-misses:u          #      6.6 %  l1d_miss_rate            ( +-  5.68% )  (46.65%)
           118,394      LLC-loads:u                      #     56.7 %  llc_miss_rate            ( +-  6.93% )  (12.84%)
            59,930      branch-misses:u                  #      2.1 %  branch_miss_rate         ( +-  7.67% )  (21.81%)
         4,397,519      branches:u                       #    111.3 M/sec  branch_frequency     ( +-  6.02% )  (20.77%)
        20,131,101      cpu-cycles:u                     #      0.5 GHz  cycles_frequency       ( +-  3.55% )  (29.82%)
        26,201,951      instructions:u                   #      1.2 instructions  insn_per_cycle  ( +-  4.13% )  (40.51%)

      34.718674823 +- 0.000585936 seconds time elapsed  ( +-  0.00% ) *)

(* Performance counter stats for './_build/default/bin/main.exe mary had a little lamb -c 10 -w 17 -e 150' (10 runs):

                 0      context-switches:u               #      0.0 cs/sec  cs_per_second     
                 0      cpu-migrations:u                 #      0.0 migrations/sec  migrations_per_second
             1,790      page-faults:u                    #  51006.6 faults/sec  page_faults_per_second  ( +-  4.42% )
             35.09 msec task-clock:u                     #      0.0 CPUs  CPUs_utilized         ( +-  1.33% )
           176,170      L1-dcache-load-misses:u          #      6.9 %  l1d_miss_rate            ( +- 12.19% )  (36.02%)
           118,900      LLC-loads:u                      #     55.5 %  llc_miss_rate            ( +- 12.90% )  (10.77%)
            74,345      branch-misses:u                  #      2.3 %  branch_miss_rate         ( +- 10.63% )  (32.19%)
         5,039,467      branches:u                       #    143.6 M/sec  branch_frequency     ( +- 12.38% )  (29.83%)
        18,887,279      cpu-cycles:u                     #      0.5 GHz  cycles_frequency       ( +-  7.49% )  (41.24%)
        23,048,472      instructions:u                   #      1.2 instructions  insn_per_cycle  ( +-  9.15% )  (53.22%)

      34.710313411 +- 0.000621945 seconds time elapsed  ( +-  0.00% ) *)

(* Performance counter stats for './_build/default/bin/main.exe mary had a little lamb -c 10 -w 17 -e 150' (10 runs):

                 0      context-switches:u               #      0.0 cs/sec  cs_per_second     
                 0      cpu-migrations:u                 #      0.0 migrations/sec  migrations_per_second
             1,985      page-faults:u                    #  49588.9 faults/sec  page_faults_per_second  ( +-  2.43% )
             40.03 msec task-clock:u                     #      0.0 CPUs  CPUs_utilized         ( +-  1.68% )
           162,292      L1-dcache-load-misses:u          #      5.6 %  l1d_miss_rate            ( +- 10.18% )  (34.22%)
            95,439      LLC-loads:u                      #     59.2 %  llc_miss_rate            ( +- 10.23% )  (9.00%)
            57,718      branch-misses:u                  #      2.5 %  branch_miss_rate         ( +- 13.02% )  (33.42%)
         3,856,802      branches:u                       #     96.3 M/sec  branch_frequency     ( +- 13.29% )  (34.49%)
        17,957,622      cpu-cycles:u                     #      0.4 GHz  cycles_frequency       ( +-  5.08% )  (42.39%)
        23,144,361      instructions:u                   #      1.1 instructions  insn_per_cycle  ( +-  7.78% )  (56.78%)

      34.721179507 +- 0.002596412 seconds time elapsed  ( +-  0.01% ) *)

(* Performance counter stats for 'zscroll mary had a little lamb -t 34.7 -l 17 -d .15 -p  ' (10 runs):

                 0      context-switches:u               #      0.0 cs/sec  cs_per_second     
                 0      cpu-migrations:u                 #      0.0 migrations/sec  migrations_per_second
             2,353      page-faults:u                    #  12030.9 faults/sec  page_faults_per_second  ( +-  0.02% )
            195.58 msec task-clock:u                     #      0.0 CPUs  CPUs_utilized         ( +-  1.23% )
         3,894,668      L1-dcache-load-misses:u          #      6.1 %  l1d_miss_rate            ( +-  1.68% )  (38.28%)
         1,901,466      LLC-loads:u                      #     31.6 %  llc_miss_rate            ( +-  3.00% )  (12.23%)
         2,033,074      branch-misses:u                  #      4.6 %  branch_miss_rate         ( +-  1.30% )  (24.72%)
        46,760,594      branches:u                       #    239.1 M/sec  branch_frequency     ( +-  2.60% )  (24.74%)
       201,168,430      cpu-cycles:u                     #      1.0 GHz  cycles_frequency       ( +-  2.19% )  (36.39%)
       228,338,894      instructions:u                   #      1.1 instructions  insn_per_cycle  ( +-  0.92% )  (49.49%)

      34.872235882 +- 0.002206712 seconds time elapsed  ( +-  0.01% ) *)

(* 
 Performance counter stats for 'sh -c echo "mary had a little lamb" | skroll  -l -r -d .15 -n 17' (10 runs):

                 0      context-switches:u               #      0.0 cs/sec  cs_per_second     
                 0      cpu-migrations:u                 #      0.0 migrations/sec  migrations_per_second
               334      page-faults:u                    #   1413.9 faults/sec  page_faults_per_second
            236.23 msec task-clock:u                     #      0.0 CPUs  CPUs_utilized       
           152,924      L1-dcache-load-misses:u          #     39.4 %  l1d_miss_rate            (27.80%)
           152,900      LLC-loads:u                      #     97.1 %  llc_miss_rate            (15.53%)
           102,343      branch-misses:u                  #     11.0 %  branch_miss_rate         (30.73%)
         1,303,071      branches:u                       #      5.5 M/sec  branch_frequency     (26.24%)
        22,892,532      cpu-cycles:u                     #      0.1 GHz  cycles_frequency       (39.90%)
         4,125,324      instructions:u                   #      0.2 instructions  insn_per_cycle  (54.56%)

     347.545158359 +- 0.000000000 seconds time elapsed *)

(* ---
minor_collections:      9
major_collections:      4
compactions:            0
forced_major_collections: 1

minor_words:    300624
promoted_words: 156652
major_words:    168310

top_heap_words: 250828
heap_words:     246409
live_words:     155224
free_words:      88589
largest_free:        0
fragments:        2596

live_blocks: 27442
free_blocks: 0
heap_chunks: 0 *)

(* Performance counter stats for './_build/default/bin/main.exe mary had a little lamb -c 800 -d bounce -w 17 -e 20':

         4,948,328      cache-references:u                 
         4,178,601      cache-misses:u                     
         1,484,166      LLC-loads:u                        
         1,266,559      LLC-load-misses:u                  

        4,573,336      cache-references:u               
         3,995,634      cache-misses:u                   
         1,248,502      LLC-loads:u                      
         1,143,969      LLC-load-misses:u  

        4,132,514      cache-references:u  
         3,502,758      cache-misses:u         
         1,053,266      LLC-loads:u            
           958,665      LLC-load-misses:u  

     161.940851705 seconds time elapsed

       0.274681000 seconds user
       0.583891000 seconds sys *)

(* Performance counter stats for './_build/default/bin/main.exe mary had a little lamb -c 800 -d bounce -w 17 -e 20':

       226,061,143      cycles:u                           
        50,717,344      instructions:u                     
         4,152,680      cache-misses:u                     
         1,261,119      LLC-load-misses:u       
         
        322,334,686      cycles:u               
        26,372,252      instructions:u                  
         4,041,087      cache-misses:u                  
         1,184,626      LLC-load-misses:u   

        188,985,098      cycles:u           
        29,178,819      instructions:u     
         3,559,543      cache-misses:u     
           980,329      LLC-load-misses:u

          1,008.21 msec task-clock:u                                                          
       265,738,382      cycles:u                                                              
        29,101,228      instructions:u                                                        
         3,571,709      cache-misses:u                                                        
           989,459      LLC-load-misses:u  



     161.752125860 seconds time elapsed

       0.243196000 seconds user
       0.616591000 seconds sys *)

(* 0.70% *)
(* 0.60% *)

(* gtl2 *)
(* Performance counter stats for './_build/default/bin/main.exe mary had a little lamb -c 1 -d bounce -w 17 -e 2':

                 0      context-switches:u               #      0.0 cs/sec  cs_per_second     
                 0      cpu-migrations:u                 #      0.0 migrations/sec  migrations_per_second
             2,078      page-faults:u                    #    155.5 faults/sec  page_faults_per_second
         13,362.51 msec task-clock:u                     #      0.1 CPUs  CPUs_utilized       
         9,218,267      branch-misses:u                  #     17.2 %  branch_miss_rate         (67.45%)
        53,934,660      branches:u                       #      4.0 M/sec  branch_frequency     (66.75%)
     4,439,806,992      cpu-cycles:u                     #      0.3 GHz  cycles_frequency       (66.26%)
       243,816,856      instructions:u                   #      0.1 instructions  insn_per_cycle  (66.13%)

     251.167726722 seconds time elapsed

       2.898982000 seconds user
       9.919058000 seconds sys *)

(* Performance counter stats for './_build/default/bin/main.exe mary had a little lamb -c 10 -w 17 -e 150' (10 runs):

            0      context-switches:u      #      0.0 cs/sec  cs_per_second
            0      cpu-migrations:u        #      0.0 migrations/sec  migrations_per_second
        1,571      page-faults:u           #  39917.4 faults/sec  page_faults_per_second  ( +-  0.03% )
        39.36 msec task-clock:u            #      0.0 CPUs  CPUs_utilized         ( +-  1.84% )
    277,758      L1-dcache-load-misses:u #      6.5 %  l1d_miss_rate            ( +- 10.92% )  (45.97%)
    115,254      LLC-loads:u             #     59.8 %  llc_miss_rate            ( +- 13.00% )  (14.94%)
    68,371      branch-misses:u         #      2.4 %  branch_miss_rate         ( +- 11.58% )  (30.33%)
    3,317,705      branches:u              #     84.3 M/sec  branch_frequency     ( +- 16.86% )  (24.24%)
16,843,137      cpu-cycles:u            #      0.4 GHz  cycles_frequency       ( +-  7.40% )  (32.26%)
18,008,144      instructions:u          #      1.0 instructions  insn_per_cycle  ( +-  7.84% )  (39.08%)

      34.717703485 +- 0.000865136 seconds time elapsed  ( +-  0.00% ) *)

(* Performance counter stats for './_build/default/bin/main.exe mary had a little lamb -c 10 -w 17 -sl 150' (10 runs):

                 0      context-switches:u               #      0.0 cs/sec  cs_per_second     
                 0      cpu-migrations:u                 #      0.0 migrations/sec  migrations_per_second
             1,662      page-faults:u                    #  45716.6 faults/sec  page_faults_per_second  ( +-  3.77% )
             36.35 msec task-clock:u                     #      0.0 CPUs  CPUs_utilized         ( +-  1.10% )
           272,213      L1-dcache-load-misses:u          #      6.0 %  l1d_miss_rate            ( +-  9.41% )  (31.85%)
           104,514      LLC-loads:u                      #     61.6 %  llc_miss_rate            ( +-  6.21% )  (7.70%)
            60,590      branch-misses:u                  #      2.6 %  branch_miss_rate         ( +-  7.95% )  (20.49%)
         2,515,208      branches:u                       #     69.2 M/sec  branch_frequency     ( +- 17.60% )  (35.89%)
        16,500,438      cpu-cycles:u                     #      0.5 GHz  cycles_frequency       ( +-  4.97% )  (48.06%)
        16,902,304      instructions:u                   #      0.9 instructions  insn_per_cycle  ( +- 12.51% )  (60.45%)

      34.565717873 +- 0.000675059 seconds time elapsed  ( +-  0.00% ) *)

(* Performance counter stats for './_build/default/bin/main.exe mary had a little lamb -c 10 -w 17 -e 150' (10 runs):

                 0      context-switches:u               #      0.0 cs/sec  cs_per_second
                 0      cpu-migrations:u                 #      0.0 migrations/sec  migrations_per_second
             2,185      page-faults:u                    #  32743.8 faults/sec  page_faults_per_second  ( +-  0.11% )
             66.73 msec task-clock:u                     #      0.0 CPUs  CPUs_utilized         ( +-  2.76% )
           509,892      L1-dcache-load-misses:u          #      6.6 %  l1d_miss_rate            ( +-  5.10% )  (42.21%)
           314,390      LLC-loads:u                      #     53.5 %  llc_miss_rate            ( +-  6.73% )  (14.14%)
           267,895      branch-misses:u                  #      3.8 %  branch_miss_rate         ( +-  3.72% )  (26.28%)
         8,073,777      branches:u                       #    121.0 M/sec  branch_frequency     ( +-  8.02% )  (22.58%)
        39,783,889      cpu-cycles:u                     #      0.6 GHz  cycles_frequency       ( +-  7.08% )  (33.54%)
        39,915,863      instructions:u                   #      1.0 instructions  insn_per_cycle  ( +-  4.08% )  (43.65%)

      34.720812527 +- 0.002001037 seconds time elapsed  ( +-  0.01% ) *)

(* strace -c ./_build/default/bin/main.exe mary had a little lamb -c 30 -d bounce -w 17 -e 10 *)
(* % time  seconds  usecs/call     calls    errors syscall
------ ----------- ----------- --------- --------- ----------------
 49.82    0.010887          36       301           clock_nanosleep
 43.05    0.009407          31       301           write
  3.71    0.000810          11        71           mmap
  1.32    0.000289         289         1           execve
  0.47    0.000102          25         4           mprotect
  0.26    0.000056          56         1           munmap
  0.24    0.000053           8         6           brk
  0.16    0.000036          12         3           openat
  0.16    0.000034          34         1           readlink
  0.11    0.000024          12         2           read
  0.09    0.000020           6         3           sigaltstack
  0.09    0.000019           6         3           close
  0.09    0.000019           6         3           fstat
  0.08    0.000017           8         2           pread64
  0.06    0.000013           4         3         3 lseek
  0.05    0.000011          11         1           newfstatat
  0.05    0.000010          10         1           getrandom
  0.04    0.000009           9         1         1 access
  0.03    0.000007           7         1           arch_prctl
  0.03    0.000006           6         1           set_tid_address
  0.03    0.000006           6         1           set_robust_list
  0.03    0.000006           6         1           prlimit64
  0.03    0.000006           6         1           rseq
  0.02    0.000005           5         1           rt_sigaction
  0.00    0.000000           0         1           getcwd
------ ----------- ----------- --------- --------- ----------------
100.00    0.021852          30       715         4 total *)

(* % time     seconds  usecs/call     calls    errors syscall
------ ----------- ----------- --------- --------- ------------------
 78.97    0.003402          11       302           io_uring_enter
 12.07    0.000520           6        80           mmap
  2.18    0.000094          23         4           munmap
  1.51    0.000065          16         4           mprotect
  1.37    0.000059          29         2           io_uring_register
  0.93    0.000040           6         6           brk
  0.72    0.000031           6         5           close
  0.67    0.000029          29         1           readlink
  0.49    0.000021           5         4         4 lseek
  0.42    0.000018           6         3           sigaltstack
  0.23    0.000010          10         1           newfstatat
  0.16    0.000007           7         1           eventfd2
  0.12    0.000005           2         2           rt_sigaction
  0.09    0.000004           4         1           getrandom
  0.07    0.000003           3         1           prlimit64
  0.00    0.000000           0         2           read
  0.00    0.000000           0         3           fstat
  0.00    0.000000           0         2           pread64
  0.00    0.000000           0         1         1 access
  0.00    0.000000           0         1           execve
  0.00    0.000000           0         1           getcwd
  0.00    0.000000           0         1           arch_prctl
  0.00    0.000000           0         1           set_tid_address
  0.00    0.000000           0         3           openat
  0.00    0.000000           0         1           set_robust_list
  0.00    0.000000           0         1           rseq
  0.00    0.000000           0         1           io_uring_setup
------ ----------- ----------- --------- --------- ------------------
100.00    0.004308           9       435         5 total *)

(* Performance counter stats for './_build/default/bin/main.exe mary had a little lamb -c 10 -w 17 -sl 150' (10 runs):

                 0      context-switches:u               #      0.0 cs/sec  cs_per_second     
                 0      cpu-migrations:u                 #      0.0 migrations/sec  migrations_per_second
             1,561      page-faults:u                    #  41419.6 faults/sec  page_faults_per_second  ( +-  0.06% )
             37.69 msec task-clock:u                     #      0.0 CPUs  CPUs_utilized         ( +-  1.28% )
           217,339      L1-dcache-load-misses:u          #      8.4 %  l1d_miss_rate            ( +-  7.55% )  (38.53%)
           149,879      LLC-loads:u                      #     55.1 %  llc_miss_rate            ( +- 10.45% )  (12.92%)
            82,263      branch-misses:u                  #      2.1 %  branch_miss_rate         ( +- 10.55% )  (21.15%)
         4,934,954      branches:u                       #    130.9 M/sec  branch_frequency     ( +-  8.15% )  (18.82%)
        18,652,798      cpu-cycles:u                     #      0.5 GHz  cycles_frequency       ( +-  5.42% )  (26.81%)
        20,780,987      instructions:u                   #      1.2 instructions  insn_per_cycle  ( +-  7.90% )  (48.56%)

      34.566471841 +- 0.000641102 seconds time elapsed  ( +-  0.00% ) *)

(* Performance counter stats for './_build/default/bin/main.exe mary had a little lamb -c 10 -w 17 -sl 150' (10 runs):

                 0      context-switches:u               #      0.0 cs/sec  cs_per_second     
                 0      cpu-migrations:u                 #      0.0 migrations/sec  migrations_per_second
             1,554      page-faults:u                    #  41755.2 faults/sec  page_faults_per_second  ( +-  0.09% )
             37.22 msec task-clock:u                     #      0.0 CPUs  CPUs_utilized         ( +-  1.18% )
           214,169      L1-dcache-load-misses:u          #      9.4 %  l1d_miss_rate            ( +-  7.57% )  (41.09%)
           126,134      LLC-loads:u                      #     54.0 %  llc_miss_rate            ( +- 12.05% )  (13.41%)
            73,280      branch-misses:u                  #      2.1 %  branch_miss_rate         ( +- 10.22% )  (23.81%)
         5,142,945      branches:u                       #    138.2 M/sec  branch_frequency     ( +-  7.39% )  (18.23%)
        18,905,542      cpu-cycles:u                     #      0.5 GHz  cycles_frequency       ( +-  5.74% )  (28.33%)
        23,454,551      instructions:u                   #      1.3 instructions  insn_per_cycle  ( +-  4.54% )  (45.50%)

      34.566038837 +- 0.000571831 seconds time elapsed  ( +-  0.00% ) *)
