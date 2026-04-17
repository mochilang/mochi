let rec add_lists a b carry =
  match a, b, carry with
  | [], [], 0 -> []
  | [], [], c -> [c]
  | x :: xs, [], c -> let s = x + c in (s mod 10) :: add_lists xs [] (s / 10)
  | [], y :: ys, c -> let s = y + c in (s mod 10) :: add_lists [] ys (s / 10)
  | x :: xs, y :: ys, c -> let s = x + y + c in (s mod 10) :: add_lists xs ys (s / 10)

let fmt xs = "[" ^ String.concat "," (List.map string_of_int xs) ^ "]"

let read_all () =
  let buf = Buffer.create 1024 in
  (try while true do Buffer.add_string buf (input_line stdin); Buffer.add_char buf ' ' done with End_of_file -> ());
  Buffer.contents buf

let rec take n xs acc =
  if n = 0 then (List.rev acc, xs) else
  match xs with
  | x :: rest -> take (n - 1) rest (int_of_string x :: acc)
  | [] -> (List.rev acc, [])

let rec solve t xs =
  if t = 0 then [] else
  match xs with
  | n :: rest ->
      let a, rest1 = take (int_of_string n) rest [] in
      (match rest1 with
       | m :: rest2 ->
           let b, rest3 = take (int_of_string m) rest2 [] in
           fmt (add_lists a b 0) :: solve (t - 1) rest3
       | [] -> [])
  | [] -> []

let () =
  match String.split_on_char ' ' (read_all ()) |> List.map String.trim |> List.filter (fun x -> x <> "") with
  | [] -> ()
  | t :: rest -> print_string (String.concat "\n" (solve (int_of_string t) rest))
