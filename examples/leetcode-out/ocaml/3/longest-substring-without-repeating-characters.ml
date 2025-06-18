exception Return_0 of int
let rec lengthOfLongestSubstring s =
  try
    let n = String.length s in
    let start = ref 0 in
    let best = ref 0 in
    let i = ref 0 in
    while !i < n do
      let j = ref !start in
      while !j < !i do
        if (String.get s !j) = (String.get s !i) then begin
          start := !j + 1;
        end;
        j := !j + 1;
      done;
      let length = !i - !start + 1 in
      if length > !best then begin
        best := length;
      end;
      i := !i + 1;
    done;
    raise (Return_0 (!best))
  with Return_0 v -> v

