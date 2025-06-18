exception Return_0 of int list
let rec addTwoNumbers l1 l2 =
  try
    let i = ref 0 in
    let j = ref 0 in
    let carry = ref 0 in
    let result = ref [] in
    while !i < List.length l1 || !j < List.length l2 || !carry > 0 do
      let x = ref 0 in
      if !i < List.length l1 then begin
        x := (List.nth l1 !i);
        i := !i + 1;
      end;
      let y = ref 0 in
      if !j < List.length l2 then begin
        y := (List.nth l2 !j);
        j := !j + 1;
      end;
      let sum = !x + !y + !carry in
      let digit = sum mod 10 in
      carry := sum / 10;
      result := !result @ [digit];
    done;
    raise (Return_0 (!result))
  with Return_0 v -> v

