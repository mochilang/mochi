type person = { name: string; age: int; status: string }

let people = [
  { name = "Alice"; age = 17; status = "minor" };
  { name = "Bob"; age = 25; status = "unknown" };
  { name = "Charlie"; age = 18; status = "unknown" };
  { name = "Diana"; age = 16; status = "minor" }
]

let updated =
  List.map (fun p ->
    if p.age >= 18 then { p with status = "adult"; age = p.age + 1 } else p)
    people

let expected = [
  { name = "Alice"; age = 17; status = "minor" };
  { name = "Bob"; age = 26; status = "adult" };
  { name = "Charlie"; age = 19; status = "adult" };
  { name = "Diana"; age = 16; status = "minor" }
]

let () =
  assert (updated = expected);
  print_endline "ok"
