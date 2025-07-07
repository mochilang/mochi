type item = { n: int; v: string }

let items = [
  { n = 1; v = "a" };
  { n = 1; v = "b" };
  { n = 2; v = "c" };
]

let result =
  List.stable_sort (fun a b -> compare a.n b.n) items
  |> List.map (fun i -> i.v)

let () =
  Printf.printf "[%s]\n" (String.concat "," result)
