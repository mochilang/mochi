partial def isMatch (s p : List Char) : Bool :=
  let sa := s.toArray
  let pa := p.toArray
  let rec go (i j : Nat) (star : Option Nat) (mt : Nat) : Bool :=
    if i < sa.size then
      if j < pa.size && ((pa.getD j ' ') = '?' || (pa.getD j ' ') = (sa.getD i ' ')) then go (i + 1) (j + 1) star mt
      else if j < pa.size && (pa.getD j ' ') = '*' then go i (j + 1) (some j) i
      else match star with | some st => go (mt + 1) (st + 1) star (mt + 1) | none => false
    else
      let rec skip (k : Nat) : Bool := if k < pa.size then if pa.getD k ' ' = '*' then skip (k + 1) else false else true
      skip j
  go 0 0 none 0

def main : IO Unit := do
  let stdin ← IO.getStdin
  let input ← stdin.readToEnd
  let lines := ((String.splitOn input "\n").map fun s => s.replace "\r" "").toArray
  if lines.isEmpty then pure () else
  if (lines.getD 0 "").trimAscii == "" then pure () else do
    let t := ((lines.getD 0 "0").trimAscii).toNat!
    let mut idx := 1
    let mut out : Array String := #[]
    for _ in [0:t] do
      let n := ((lines.getD idx "0").trimAscii).toNat!; idx := idx + 1
      let s := if n > 0 then lines.getD idx "" else ""
      if n > 0 then idx := idx + 1
      let m := ((lines.getD idx "0").trimAscii).toNat!; idx := idx + 1
      let p := if m > 0 then lines.getD idx "" else ""
      if m > 0 then idx := idx + 1
      out := out.push (if isMatch s.toList p.toList then "true" else "false")
    IO.println (String.intercalate "\n" out.toList)
