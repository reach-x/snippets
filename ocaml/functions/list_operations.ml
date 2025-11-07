(* List operations in OCaml *)

let demo () =
  print_endline "\n=== List Operations in OCaml ===\n";

  let numbers = [1; 2; 3; 4; 5] in
  Printf.printf "Numbers: [%s]\n" (String.concat "; " (List.map string_of_int numbers));

  (* List length *)
  Printf.printf "Length: %d\n" (List.length numbers);

  (* Head and tail *)
  Printf.printf "Head: %d\n" (List.hd numbers);
  Printf.printf "Tail: [%s]\n" (String.concat "; " (List.map string_of_int (List.tl numbers)));

  (* Prepend *)
  let prepended = 0 :: numbers in
  Printf.printf "\nPrepend 0: [%s]\n" (String.concat "; " (List.map string_of_int prepended));

  (* Append *)
  let appended = numbers @ [6; 7; 8] in
  Printf.printf "Append: [%s]\n" (String.concat "; " (List.map string_of_int appended));

  (* Map *)
  let squared = List.map (fun x -> x * x) numbers in
  Printf.printf "\nMap (square): [%s]\n" (String.concat "; " (List.map string_of_int squared));

  (* Filter *)
  let evens = List.filter (fun x -> x mod 2 = 0) numbers in
  Printf.printf "Filter (even): [%s]\n" (String.concat "; " (List.map string_of_int evens));

  (* Fold *)
  let sum = List.fold_left (+) 0 numbers in
  Printf.printf "\nFold (sum): %d\n" sum;

  (* Reverse *)
  let reversed = List.rev numbers in
  Printf.printf "Reversed: [%s]\n" (String.concat "; " (List.map string_of_int reversed))

let () = demo ()
