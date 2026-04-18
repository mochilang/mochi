let read_all ic = let b = Buffer.create 1024 in (try while true do Buffer.add_channel b ic 1 done with End_of_file -> ()); Buffer.contents b
let solve_case s =
  let stack = Stack.create () in Stack.push (-1) stack; let best = ref 0 in
  for i = 0 to String.length s - 1 do
    if s.[i] = '(' then Stack.push i stack
    else begin ignore (Stack.pop stack); if Stack.is_empty stack then Stack.push i stack else best := max !best (i - Stack.top stack) end
  done; !best
let () = let arr = Array.of_list (String.split_on_char '\n' (read_all stdin)) in if Array.length arr = 0 || String.trim arr.(0) = "" then () else let idx = ref 0 in let next () = let v = if !idx < Array.length arr then String.trim arr.(!idx) else "" in incr idx; v in let t = int_of_string (next ()) in let out = ref [] in for _ = 1 to t do let n = int_of_string (next ()) in let s = if n > 0 then next () else "" in out := !out @ [string_of_int (solve_case s)] done; print_string (String.concat "\n" !out)
