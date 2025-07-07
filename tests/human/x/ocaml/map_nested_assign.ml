let data : (string, (string, int) Hashtbl.t) Hashtbl.t = Hashtbl.create 10
let () =
  let inner = Hashtbl.create 1 in
  Hashtbl.add inner "inner" 1;
  Hashtbl.add data "outer" inner;
  Hashtbl.replace (Hashtbl.find data "outer") "inner" 2;
  print_endline (string_of_int (Hashtbl.find (Hashtbl.find data "outer") "inner"))
