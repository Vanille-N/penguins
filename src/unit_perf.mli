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

(** {2 Example usage} *)

(** {[
    Unit_bench.(
         perf "iterative" (fun () ->
            for i = 0 to 10000000 do
                ()
            done
        );
        perf ~repeat:20 "recursive" (fun () ->
            let rec loop = function
                | 0 -> ()
                | n -> loop (n-1)
            in loop 10000000 
        );
        report ()
    )
    ]}
    
    yields
    
    {[
    <============================={ START }=============================>

    perf::Loops
      * iterative   ..................... 0.81540 (10 iter)
      * recursive   ..................... 1.70818 (20 iter)

        *-- Summary: 2 performance measurements --*

    <=============================={ END }==============================>
    ]}

    *)


