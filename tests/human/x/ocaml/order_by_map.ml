type item = { a: int; b: int }

let data = [
  { a = 1; b = 2 };
  { a = 1; b = 1 };
  { a = 0; b = 5 };
]

let sorted = List.sort (fun x y -> compare (x.a, x.b) (y.a, y.b)) data

let () =
  List.iter (fun i -> Printf.printf "{a=%d; b=%d}\n" i.a i.b) sorted
