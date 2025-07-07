let x = 2

let label =
  match x with
  | 1 -> "one"
  | 2 -> "two"
  | 3 -> "three"
  | _ -> "unknown"

let day = "sun"

let mood =
  match day with
  | "mon" -> "tired"
  | "fri" -> "excited"
  | "sun" -> "relaxed"
  | _ -> "normal"

let ok = true

let status = if ok then "confirmed" else "denied"

let classify n =
  match n with
  | 0 -> "zero"
  | 1 -> "one"
  | _ -> "many"

let () =
  print_endline label;
  print_endline mood;
  print_endline status;
  print_endline (classify 0);
  print_endline (classify 5)
