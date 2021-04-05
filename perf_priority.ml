module Ints : (Priority.ORDERED with type t = int) = struct
    type t = int
    let compare = compare
end

module PQueue = Priority.Make(Ints)

open Unit_perf

let main () = PQueue.(
    let n = 1000 in
    let data = List.init n (fun _ -> Random.int 10000) in
    perf ~repeat:1000 "many insertions" (fun () ->
        let pq = create n 0 0 in
        List.iter (fun i -> let _ = insert pq i i in ()) data
    );
    perf ~repeat:100 "insert and extract" (fun () ->
        let pq = create n 0 0 in
        List.iter (fun i -> let _ = insert pq i i in ()) data;
        for i = 1 to n do let _ = extract_min pq in () done
    );
    perf ~repeat:10 "insert and member" (fun () ->
        let pq = create n 0 0 in
        let nodes = List.map (fun i -> insert pq i i) data in
        List.iter (fun n -> let _ = member pq n in ()) nodes
    )
)

let () =
    init "Priority";
    main ();
    report ()
