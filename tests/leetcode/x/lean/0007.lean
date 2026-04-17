def reverseInt (x : Int) : Int :=
  let rec go (x ans : Int) : Int :=
    if x = 0 then ans
    else
      let digit := x % 10
      let x := x / 10
      if ans > 214748364 ∨ (ans = 214748364 ∧ digit > 7) then 0
      else if ans < -214748364 ∨ (ans = -214748364 ∧ digit < -8) then 0
      else go x (ans * 10 + digit)
  go x 0

def main : IO Unit := do
  let input <- IO.getStdin.readToEnd
  let lines := input.splitOn "\n"
  if lines.isEmpty then pure () else do
    let t := (lines.head!.trim).toNat!
    let out := (List.range t).map fun i => toString (reverseInt ((lines.getD (i + 1) "0").trim.toInt!))
    IO.println (String.intercalate "\n" out)
