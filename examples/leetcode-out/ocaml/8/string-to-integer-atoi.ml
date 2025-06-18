exception Return_0 of int
let rec myAtoi s =
  try
    let i = ref 0 in
    let n = String.length s in
    while !i < n && (String.get s !i) = " " do
      i := !i + 1;
    done;
    let sign = ref 1 in
    if !i < n && ((String.get s !i) = "+" || (String.get s !i) = "-") then begin
      if (String.get s !i) = "-" then begin
        sign := -1;
      end;
      i := !i + 1;
    end;
    let digits = (let tbl = Hashtbl.create 10 in Hashtbl.add tbl "0" 0;;Hashtbl.add tbl "1" 1;;Hashtbl.add tbl "2" 2;;Hashtbl.add tbl "3" 3;;Hashtbl.add tbl "4" 4;;Hashtbl.add tbl "5" 5;;Hashtbl.add tbl "6" 6;;Hashtbl.add tbl "7" 7;;Hashtbl.add tbl "8" 8;;Hashtbl.add tbl "9" 9; tbl) in
    let result = ref 0 in
    while !i < n do
      let ch = (String.get s !i) in
      if !(Hashtbl.mem digits ch) then begin
      end;
      let d = (List.nth digits ch) in
      result := !result * 10 + d;
      i := !i + 1;
    done;
    result := !result * !sign;
    if !result > 2147483647 then begin
      raise (Return_0 (2147483647))
    end;
    if !result < (-2147483648) then begin
      raise (Return_0 (-2147483648))
    end;
    raise (Return_0 (!result))
  with Return_0 v -> v

