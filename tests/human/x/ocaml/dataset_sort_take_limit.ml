let products = [
  ("Laptop", 1500);
  ("Smartphone", 900);
  ("Tablet", 600);
  ("Monitor", 300);
  ("Keyboard", 100);
  ("Mouse", 50);
  ("Headphones", 200)
]

let sorted = List.sort (fun (_,p1) (_,p2) -> compare p2 p1) products

let rec take n lst = match n, lst with
  | 0, _ | _, [] -> []
  | n, x::xs -> x :: take (n-1) xs

let expensive = take 3 (List.tl sorted)

let () =
  print_endline "--- Top products (excluding most expensive) ---";
  List.iter (fun (name,price) -> Printf.printf "%s costs $ %d\n" name price) expensive
