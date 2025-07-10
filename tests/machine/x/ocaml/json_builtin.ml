type record1 = { mutable a : int; mutable b : int }

let m : (string * Obj.t) list = { a = 1; b = 2 }

let () =
  json m;
