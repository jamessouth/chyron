open Core

let profile_startup_ns f =
  let start_time = Time_ns.now () in
  let result = f () in
  let end_time = Time_ns.now () in
  let diff = Time_ns.diff end_time start_time in
  (* Convert directly to an unboxed integer of microseconds *)
  let us = Time_ns.Span.to_int_us diff in
  (result, us)

type gc_snapshot_words = {
  minor_words_allocated : int;
  major_words_allocated : int;
}

let profile_allocation_precise f =
  (* Flush out pending dead objects before starting *)
  Gc.compact ();

  let start_stats = Gc.quick_stat () in
  let result = f () in
  let end_stats = Gc.quick_stat () in

  (* Perform integer subtraction to avoid float boxing allocations *)
  let snapshot =
    {
      minor_words_allocated =
        Float.iround_towards_zero_exn
          (end_stats.minor_words -. start_stats.minor_words);
      major_words_allocated =
        Float.iround_towards_zero_exn
          (end_stats.major_words -. start_stats.major_words);
    }
  in
  (result, snapshot)

type gc_snapshot = { minor_alloc : float; major_alloc : float }

let profile_allocation f =
  Gc.full_major ();
  let start_stats = Gc.quick_stat () in
  let result = f () in
  Gc.minor ();
  let end_stats = Gc.quick_stat () in
  let snapshot =
    {
      minor_alloc = end_stats.minor_words -. start_stats.minor_words;
      major_alloc = end_stats.major_words -. start_stats.major_words;
    }
  in
  (result, snapshot)
