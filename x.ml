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
  p [%fmt "$story"]

let () =
  p [%fmt "${1, %d}"]

let () =
  p [%fmt {|${x, string_formatter}|}]

let () =
  p [%fmt "${int, int_formatter} looks different than ${float, float_formatter}!"]

let () = p [%fmt
{xxx|This is a $story
That is still being ${told, %s}
But before we get ${"started", string_formatter}
We're all gonna get $$old|xxx}
]

let f () () =
  Printf.ksprintf print_endline

let () =
  f () () [%fmt "$x"]

let g () ~fmt =
  Printf.ksprintf print_endline fmt

let () =
  g () ~fmt:[%fmt "$story"]
