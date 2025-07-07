let numbers = [1;2;3;4;5;6;7;8;9]
let rec loop lst =
  match lst with
  | [] -> ()
  | n::rest ->
      if n mod 2 = 0 then loop rest
      else if n > 7 then ()
      else (
        print_endline ("odd number: " ^ string_of_int n);
        loop rest
      )
let () = loop numbers
