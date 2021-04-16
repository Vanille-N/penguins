(** Basic tests *)

(** Start the tester with a label *)
val init : string -> unit

(** Check the correct execution of a [unit -> unit] function

    The string provides the label the test is registered under *)
val test : string -> (unit -> unit) -> unit

(** Summary of tests *)
val report : unit -> unit
