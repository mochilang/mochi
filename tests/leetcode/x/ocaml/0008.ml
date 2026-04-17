let my_atoi s =
  let n = String.length s in
  let i = ref 0 in
  while !i < n && s.[!i] = ' ' do incr i done;
  let sign = ref 1 in
  if !i < n && (s.[!i] = '+' || s.[!i] = '-') then begin
    if s.[!i] = '-' then sign := -1;
    incr i
  end;
  let ans = ref 0 in
  let limit = if !sign > 0 then 7 else 8 in
  while !i < n && s.[!i] >= '0' && s.[!i] <= '9' do
    let digit = Char.code s.[!i] - Char.code '0' in
    if !ans > 214748364 || (!ans = 214748364 && digit > limit) then begin
      ans := if !sign > 0 then 2147483647 else 2147483648;
      i := n
    end else begin
      ans := !ans * 10 + digit;
      incr i
    end
  done;
  !sign * !ans

let () =
  try
    let t = int_of_string (String.trim (read_line ())) in
    let out = ref [] in
    for _ = 1 to t do
      let s = read_line () in
      out := string_of_int (my_atoi s) :: !out
    done;
    print_string (String.concat "\n" (List.rev !out))
  with End_of_file -> ()
