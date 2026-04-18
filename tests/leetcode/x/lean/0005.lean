def expand (s : String) (left right : Int) : Int × Int :=
  if h1 : left >= 0 ∧ right < s.length then
    let li := String.get! s left.toNat
    let ri := String.get! s right.toNat
    if li = ri then
      expand s (left - 1) (right + 1)
    else
      (left + 1, right - left - 1)
  else
    (left + 1, right - left - 1)

def longestPalindrome (s : String) : String :=
  let n := s.length
  let rec loop (i : Nat) (bestStart bestLen : Int) : String :=
    if h : i < n then
      let (s1, l1) := expand s i i
      let (bestStart, bestLen) := if l1 > bestLen then (s1, l1) else (bestStart, bestLen)
      let (s2, l2) := expand s i (i + 1)
      let (bestStart, bestLen) := if l2 > bestLen then (s2, l2) else (bestStart, bestLen)
      loop (i + 1) bestStart bestLen
    else
      s.extract bestStart.toNat (bestStart + bestLen).toNat
  loop 0 0 (if n = 0 then 0 else 1)

def main : IO Unit := do
  let input <- IO.getStdin.readToEnd
  let lines := input.splitOn "
"
  if lines.isEmpty then
    pure ()
  else do
    let t := (lines.head!.trim).toNat!
    let strs := (List.range t).map fun i => ((lines.getD (i + 1) "").trimRight)
    IO.println (String.intercalate "
" (strs.map longestPalindrome))
