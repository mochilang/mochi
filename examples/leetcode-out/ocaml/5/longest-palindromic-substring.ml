exception Return_0 of int
let rec expand s left right =
  try
    let l = ref left in
    let r = ref right in
    let n = String.length s in
    while !l >= 0 && !r < n do
      if (String.get s !l) <> (String.get s !r) then begin
      end;
      l := !l - 1;
      r := !r + 1;
    done;
    raise (Return_0 (!r - !l - 1))
  with Return_0 v -> v

exception Return_1 of string
let rec longestPalindrome s =
  try
    if String.length s <= 1 then begin
      raise (Return_1 (s))
    end;
    let start = ref 0 in
    let _end = ref 0 in
    let n = String.length s in
    for i = 0 to n - 1 do
      let len1 = expand s i i in
      let len2 = expand s i i + 1 in
      let l = ref len1 in
      if len2 > len1 then begin
        l := len2;
      end;
      if !l > (!_end - !start) then begin
        start := i - ((!l - 1) / 2);
        _end := i + (!l / 2);
      end;
    done;
    let res = ref "" in
    let k = ref !start in
    while !k <= !_end do
      res := !res ^ (String.make 1 (String.get s !k));
      k := !k + 1;
    done;
    raise (Return_1 (!res))
  with Return_1 v -> v

