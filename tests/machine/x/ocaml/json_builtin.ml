type record1 = { mutable a : int; mutable b : int }

let m : record1 = { a = 1; b = 2 }

let () =
  json m;
