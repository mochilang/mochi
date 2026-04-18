def solveCase (s : String) : Nat := Id.run do
  let chars := s.toList.toArray
  let mut stack : Array Int := #[-1]
  let mut best : Nat := 0
  for i in [0:chars.size] do
    if chars.getD i ' ' = '(' then
      stack := stack.push i
    else
      stack := stack.pop
      if stack.isEmpty then
        stack := stack.push i
      else
        let last := stack.getD (stack.size - 1) 0
        let span : Nat := Int.natAbs (i - last)
        if span > best then best := span
  return best

def main : IO Unit := do
  let stdin ← IO.getStdin
  let input ← stdin.readToEnd
  let lines := ((String.splitOn input "\n").map fun s => (s.trimAscii).toString).toArray
  if lines.isEmpty then pure () else do
    let t := (lines.getD 0 "0").toNat!
    let mut idx := 1
    let mut out : Array String := #[]
    for _ in [0:t] do
      let n := (lines.getD idx "0").toNat!
      idx := idx + 1
      let s := if n > 0 then lines.getD idx "" else ""
      if n > 0 then idx := idx + 1
      out := out.push (toString (solveCase s))
    IO.println (String.intercalate "\n" out.toList)
