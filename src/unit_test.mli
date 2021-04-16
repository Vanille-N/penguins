(** Basic tests *)

(** Start the tester with a label *)
val init : string -> unit

(** Check the correct execution of a [unit -> unit] function

    The string provides the label the test is registered under *)
val test : string -> (unit -> unit) -> unit

(** Summary of tests *)
val report : unit -> unit

(** {2 Example usage} *)

(** {[
    Unit_test.(
        init "Integers";
        test "simple addition" (fun () ->
            assert (1 + 1 = 2)
        );
        test "simple multiplication" (fun () ->
            assert (3 * 2 = 6)
        );
        test "wrong subtraction" (fun () ->
            assert (9 - 1 = 7);
        );
        test "dangerous division" (fun () ->
            assert (0 / 0 = 1);
        );
        report ()
    )
    ]}

    yields

    {[
    <============================={ START }=============================>

    test::Integers
      * simple addition   .............................................OK
      * simple multiplication   .......................................OK
      * wrong subtraction   ...........................................FAILURE
      * dangerous division   ..........................................FAILURE

        *-- Summary: 4 tests, 2 successes, 2 failures --*
    In test <dangerous division>
      Other exception: Division_by_zero
    In test <wrong subtraction>
      Assert failed: //toplevel// at (9,0)

    <=============================={ END }==============================>
    ]}
    *)
