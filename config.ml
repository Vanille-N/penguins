let debug = ref false
let display = ref true
let ansi_fmt = ref true
let quiet = ref false

let first_pass = ref true
let extremal_pass = ref true
let trim_general = ref true
let trim_single = ref true

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
