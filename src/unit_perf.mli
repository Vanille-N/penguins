(** Basic benchmarks *)

(** Start the bencher with a label *)
val init : string -> unit

(** Measure one [unit -> unit] function
    
    [repeat] is the number of iterations that are averaged together: default 10

    The string parameter gives the name associated with the benchmark

    NOTE: This function measures processor time, not user time *)
val perf : ?repeat:int -> string -> (unit -> unit) -> unit

(** Summary of benchmarks *)
val report : unit -> unit
