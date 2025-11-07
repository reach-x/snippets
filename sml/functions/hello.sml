(* Standard ML example *)
fun factorial 0 = 1
  | factorial n = n * factorial (n - 1);

fun main () = (
  print "Hello from Standard ML!\n";
  print (Int.toString (factorial 5) ^ "\n")
);
