let calculate expr =
  let result = ref 0 in
  let number = ref 0 in
  let sign = ref 1 in
  let stack = Stack.create () in
  String.iter
    (fun ch ->
      if ch >= '0' && ch <= '9' then
        number := (!number * 10) + (Char.code ch - Char.code '0')
      else if ch = '+' || ch = '-' then (
        result := !result + (!sign * !number);
        number := 0;
        sign := if ch = '+' then 1 else -1
      ) else if ch = '(' then (
        Stack.push !result stack;
        Stack.push !sign stack;
        result := 0;
        number := 0;
        sign := 1
      ) else if ch = ')' then (
        result := !result + (!sign * !number);
        number := 0;
        let prev_sign = Stack.pop stack in
        let prev_result = Stack.pop stack in
        result := prev_result + (prev_sign * !result)
      ))
    expr;
  !result + (!sign * !number)

let () =
  try
    let t = int_of_string (read_line ()) in
    for i = 0 to t - 1 do
      let expr = read_line () in
      if i > 0 then print_newline ();
      print_int (calculate expr)
    done
  with End_of_file -> ()
