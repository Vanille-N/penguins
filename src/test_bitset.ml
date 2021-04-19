(** Unit tests for {!Bitset}

    Only tests basic operations :
    neither {!Bitset.SET.transitive_closure} nor {!Bitset.SET.union}, {!Bitset.SET.intersect}, {!Bitset.SET.setminus}
    are verified. This is somewhat justified by their implementation being
    less easy to verify and less prone to low-level mistakes.

    Mostly {!Bitset.SET.empty}, {!Bitset.SET.add}, {!Bitset.SET.remove} are verified since they are both
    prone to edge case mistakes and central to everything else. {!Bitset.SET.subset}
    is presumed correct *)

module Chars : (Bitset.FIN with type t = char) = struct
    type t = char
    let max = 256
    let to_int = int_of_char
    let of_int = char_of_int
end
module CSet = Bitset.Make(Chars)

open Unit_test

let main () = CSet.(
    let alpha = function 'a'..'z'|'A'..'Z' -> true | _ -> false in
    let numeric = function '0'..'9' -> true | _ -> false in
    let punct = function '.'|','|'?'|':'|';'|'!' -> true | _ -> false in
    let alphanumeric c = alpha c || numeric c in
    let space = ' ' in
    let minimum = '\000' in
    let maximum = '\255' in
    test "empty is subset of itself"
        (fun () -> assert (subset empty empty));
    test "empty does not contain elem"
        (fun () -> assert (not (member empty space)));
    test "empty has cardinal 0"
        (fun () -> assert (cardinal empty = 0));
    test "empty is subset of anything"
        (fun () -> assert (subset empty (init alpha)));
    test "empty is superset of nothing"
        (fun () -> assert (not (subset (init alpha) empty)));
    test "alpha contains letters" (fun () ->
        assert (member (init alpha) 'a');
        assert (member (init alpha) 'Z')
    );
    test "alpha contains only letters"
        (fun () -> assert (not (member (init alpha) '?')));
    test "alpha subset of alphanumeric"
        (fun () -> assert (subset (init alpha) (init alphanumeric)));
    test "alpha not superset of alphanumeric"
        (fun () -> assert (not (subset (init alphanumeric) (init alpha))));
    test "add numbers to alpha" (fun () ->
        let a = init alpha in
        let a = add a '0' in
        let a = add a '1' in
        let a = add a '2' in 
        assert (subset a (init alphanumeric));
        assert (subset (init alpha) a);
        assert (not (subset a (init alpha)));
        assert (not (subset (init alphanumeric) a))
    );
    test "identical init is equal" (fun () ->
        let a = init alpha and b = init alpha in
        assert (subset a b);
        assert (subset b a)
    );
    test "interface is immutable" (fun () ->
        let a = init alpha in
        let _ = add a '0' in
        assert (not (member a '0'));
        let _ = remove a 'a' in
        assert (member a 'a');
    );
    test "add and remove are opposites" (fun () ->
        let a = init alpha in
        let b = add a '0' in
        let c = remove b '0' in
        assert (subset a b);
        assert (subset c b);
        assert (subset a c);
        assert (subset c a)
    );
    test "init cardinalities" (fun () ->
        assert (cardinal (init alpha) = 26 * 2);
        assert (cardinal (init numeric) = 10);
        assert (cardinal (init punct) = 6)
    );
    test "add increments cardinality" (fun () ->
        let a = (init alpha) in
        let b = add a '0' in
        let c = add a 'a' in
        assert (cardinal a = cardinal c);
        assert (cardinal a + 1 = cardinal b)
    );
    test "remove decrements cardinality" (fun () ->
        let a = (init alpha) in
        let b = remove a '0' in
        let c = remove a 'a' in
        assert (cardinal a = cardinal b);
        assert (cardinal a = cardinal c + 1)
    );
    test "iter covers all" (fun () ->
        let a = ref 0 in
        iter (init alpha) (fun _ -> incr a);
        assert (!a = 26 * 2)
    );
    test "iter zero times" (fun () ->
        iter empty (fun _ -> assert false);
    );
    test "iter is ordered" (fun () ->
        let a = ref ' ' in
        iter (init numeric) (fun c -> a := c);
        assert (!a = '9')
    );
    test "edge conditions" (fun () ->
        let e = empty in
        let maxi = add e maximum in
        let mini = add e minimum in
        let nomaxi = remove maxi maximum in
        let nomini = remove mini minimum in
        assert (cardinal nomaxi = 0);
        assert (cardinal nomini = 0);
        assert (cardinal maxi = 1);
        assert (cardinal mini = 1);
        assert (member maxi maximum);
        assert (member mini minimum);
        assert (not (member nomaxi maximum));
        assert (not (member nomini minimum))
    );
    test "exhaustive check" (fun () ->
        let curr = ref empty in
        for i = 0 to 255 do
            let prev = !curr in
            let c = char_of_int i in
            curr := add prev c;
            assert (not (member prev c));
            assert (member !curr c);
            assert (cardinal !curr = cardinal prev + 1);
        done;
        for i = 0 to 255 do
            let prev = !curr in
            let c = char_of_int i in
            curr := remove prev c;
            assert (member prev c);
            assert (not (member !curr c));
            assert (cardinal !curr + 1 = cardinal prev);
        done
    );
    test "collect" (fun () ->
        assert (collect (init numeric) = List.init 10 (fun i -> char_of_int (i + int_of_char '0')));
        assert (collect (init alpha) = (
            List.init 26 (fun i -> char_of_int (i + int_of_char 'A'))
            @ List.init 26 (fun i -> char_of_int (i + int_of_char 'a'))
        ));
    )
)

let () =
    init "Bitset";
    main ();
    report ()
