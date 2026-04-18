def myAtoi (s : String) : Int :=
  let rec skip (i : Nat) : Nat := if i < s.length ∧ s.get ⟨i, by omega⟩ = ' ' then skip (i + 1) else i
  let i := skip 0
  let (sign, i) :=
    if i < s.length ∧ s.get ⟨i, by omega⟩ = '-' then (-1, i + 1)
    else if i < s.length ∧ s.get ⟨i, by omega⟩ = '+' then (1, i + 1)
    else (1, i)
  let limit := if sign = 1 then 7 else 8
  let rec parse (j : Nat) (ans : Int) : Int :=
    if j < s.length then
      let ch := s.get ⟨j, by omega⟩
      if '0' ≤ ch ∧ ch ≤ '9' then
        let digit := ch.toNat - '0'.toNat
        if ans > 214748364 ∨ (ans = 214748364 ∧ digit > limit) then if sign = 1 then 2147483647 else -2147483648
        else parse (j + 1) (ans * 10 + digit)
      else sign * ans
    else sign * ans
  parse i 0

def main : IO Unit := do
  let input <- IO.getStdin.readToEnd
  let lines := input.splitOn "\n"
  if lines.isEmpty then pure () else do
    let t := (lines.head!.trim).toNat!
    let out := (List.range t).map fun i => toString (myAtoi ((lines.getD (i + 1) "").trimRight))
    IO.println (String.intercalate "\n" out)
