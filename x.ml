let p = Printf.printf
let x = "X"
let string_formatter oc x = output_string oc x
let int = 1
let int_formatter oc x = Printf.fprintf oc "%d" x
let float = 2.0
let float_formatter oc x = Printf.fprintf oc "%g" x
let story = "STORY"
let told = "TOLD"
let started = "STARTED"

let () =
  [%fmt p "$story"]

let () =
  [%fmt p "${1, %d}"]

let () =
  [%fmt p {|${x, string_formatter}|}]

let () =
  [%fmt p "${int, int_formatter} looks different than ${float, float_formatter}!"]

let () = [%fmt p
{xxx|This is a $storyy
That is still being ${toldd, %s}
But before we get ${"started", string_formatter}
We're all gonna get $$old|xxx}
]

let f () () =
  Printf.ksprintf print_endline

let () =
  [%fmt f () () "$x"]
