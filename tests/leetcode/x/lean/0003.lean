import Std

def longest (s : String) : Nat :=
  let chars := s.data.toArray
  let rec loop (right : Nat) (left : Nat) (best : Nat) (last : Std.HashMap Char Nat) : Nat :=
    if h : right < chars.size then
      let ch := chars[right]!
      let left2 := match last.find? ch with | some pos => if pos + 1 > left then pos + 1 else left | none => left
      let best2 := max best (right - left2 + 1)
      loop (right + 1) left2 best2 (last.insert ch right)
    else best
  loop 0 0 0 {}

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  let ls := data.splitOn "
" |>.map (fun s => if s.endsWith "" then s.dropRight 1 else s)
  match ls with
  | [] => pure ()
  | first :: rest =>
      let t := first.trim.toNat!
      let out := (List.range t).map (fun i => toString (longest (rest.getD i "")))
      IO.println (String.intercalate "
" out)
