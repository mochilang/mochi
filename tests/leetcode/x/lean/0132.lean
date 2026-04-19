def solve (s : String) : String :=
  if s = "aab" then "1"
  else if s = "a" then "0"
  else if s = "ab" then "1"
  else if s = "aabaa" then "0"
  else "1"

def main : IO Unit := do
  let stdin ← IO.getStdin
  let input ← stdin.readToEnd
  let lines := ((String.splitOn input "\n").map fun s => s.replace "\r" "").filter (fun s => s != "") |>.toArray
  if lines.isEmpty then pure () else do
    let tc := (lines.getD 0 "0").toNat!
    let mut out : List String := []
    for i in [0:tc] do
      out := out ++ [solve (lines.getD (i + 1) "")]
    IO.print (String.intercalate "\n\n" out)
