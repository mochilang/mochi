exception Return_0 of bool
let rec isPalindrome x =
  try
    if x < 0 then begin
      raise (Return_0 (false))
    end;
    let s = string_of_int x in
    let n = List.length s in
    for i = 0 to n / 2 - 1 do
      if (List.nth s i) <> (List.nth s n - 1 - i) then begin
        raise (Return_0 (false))
      end;
    done;
    raise (Return_0 (true))
  with Return_0 v -> v

