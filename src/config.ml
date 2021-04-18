let debug = ref false
let display = ref true
let quiet = ref false
let show_steps = ref true

let first_pass = ref true
let extremal_pass = ref true
let trim = ref true

let inspect ?b:(b=true) fn lst =
    if b then List.iter fn lst;
    lst

let passthrough ?b:(b=true) fn x =
    if b then fn x;
    x
