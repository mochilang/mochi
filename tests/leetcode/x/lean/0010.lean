partial def matchAt (s p : Array Char) (i j : Nat) : Bool :=
  if j >= p.size then
    i >= s.size
  else
    let first := i < s.size && (p.getD j ' ' = '.' || s.getD i ' ' = p.getD j ' ')
    if j + 1 < p.size && p.getD (j + 1) ' ' = '*' then
      matchAt s p i (j + 2) || (first && matchAt s p (i + 1) j)
    else
      first && matchAt s p (i + 1) (j + 1)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let input ← stdin.readToEnd
  let lines := (((String.splitOn input "\n").map fun s => (s.trimAscii).toString).filter (fun line => line != "")).toArray
  if lines.isEmpty then
    pure ()
  else do
    let t := (lines.getD 0 "0").toNat!
    let mut idx := 1
    let mut out : Array String := #[]
    for _ in [0:t] do
      let s := (lines.getD idx "").toList.toArray
      let p := (lines.getD (idx + 1) "").toList.toArray
      idx := idx + 2
      out := out.push (if matchAt s p 0 0 then "true" else "false")
    IO.println (String.intercalate "\n" out.toList)
