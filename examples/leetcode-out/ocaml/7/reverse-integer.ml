exception Return_0 of int
let rec reverse x =
  try
    let sign = ref 1 in
    let n = ref x in
    if !n < 0 then begin
      sign := -1;
      n := -!n;
    end;
    let rev = ref 0 in
    while !n <> 0 do
      let digit = !n mod 10 in
      rev := !rev * 10 + digit;
      n := !n / 10;
    done;
    rev := !rev * !sign;
    if !rev < (-2147483647 - 1) || !rev > 2147483647 then begin
      raise (Return_0 (0))
    end;
    raise (Return_0 (!rev))
  with Return_0 v -> v

