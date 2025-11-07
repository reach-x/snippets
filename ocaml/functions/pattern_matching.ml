(* Pattern matching and types in OCaml *)

type shape =
  | Circle of float
  | Rectangle of float * float
  | Triangle of float * float

let area = function
  | Circle r -> 3.14159 *. r *. r
  | Rectangle (w, h) -> w *. h
  | Triangle (b, h) -> 0.5 *. b *. h

let rec factorial = function
  | 0 | 1 -> 1
  | n -> n * factorial (n - 1)

let rec sum_list = function
  | [] -> 0
  | head :: tail -> head + sum_list tail

let demo () =
  print_endline "\n=== Pattern Matching in OCaml ===\n";
  Printf.printf "Circle area: %.2f\n" (area (Circle 5.0));
  Printf.printf "Factorial 5: %d\n" (factorial 5);
  Printf.printf "Sum [1;2;3;4;5]: %d\n" (sum_list [1;2;3;4;5])

let () = demo ()
