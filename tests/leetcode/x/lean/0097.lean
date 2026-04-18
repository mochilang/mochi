import Std

private def solve (s1 s2 s3 : String) : Bool :=
  let a1 := s1.toList.toArray
  let a2 := s2.toList.toArray
  let a3 := s3.toList.toArray
  let m := a1.size
  let n := a2.size
  if m + n != s3.length then
    false
  else
    let rec cell (i j : Nat) : Bool :=
      if i = 0 && j = 0 then
        true
      else
        let use1 := if i > 0 then cell (i - 1) j && a1.getD (i - 1) ' ' = a3.getD (i + j - 1) ' ' else false
        let use2 := if j > 0 then cell i (j - 1) && a2.getD (j - 1) ' ' = a3.getD (i + j - 1) ' ' else false
        use1 || use2
    cell m n

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  let lines := ((String.splitOn data "\n").map fun s => s.replace "\r" "").toArray
  if lines.isEmpty || lines.getD 0 "" = "" then pure () else
    let t := ((lines.getD 0 "0").trimAscii).toNat!
    let mut out : Array String := #[]
    for i in List.range t do
      out := out.push (if solve (lines.getD (1 + 3 * i) "") (lines.getD (2 + 3 * i) "") (lines.getD (3 + 3 * i) "") then "true" else "false")
    IO.print (String.intercalate "\n" out.toList)
