exception Return_0 of int list
let rec twoSum nums target =
  try
    let n = List.length nums in
    for i = 0 to n - 1 do
      for j = i + 1 to n - 1 do
        if (List.nth nums i) + (List.nth nums j) = target then begin
          raise (Return_0 ([i; j]))
        end;
      done;
    done;
    raise (Return_0 ([-1; -1]))
  with Return_0 v -> v

let result = twoSum [2; 7; 11; 15] 9;;
print_endline (string_of_int ((List.nth result 0)));;
print_endline (string_of_int ((List.nth result 1)));;
