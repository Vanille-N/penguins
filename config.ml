let debug = ref false
let display = ref false
let ansi_fmt = ref false

let rec any fn = function
    | [] -> false
    | hd :: _ when fn hd -> true
    | _ :: tl -> any fn tl

let rec all fn = function
    | [] -> true
    | hd :: _ when not (fn hd) -> false
    | _ :: tl -> all fn tl

let inspect ?b:(b=true) fn lst =
    if b then List.iter fn lst;
    lst

let passthrough ?b:(b=true) fn x =
    if b then fn x;
    x
