import Std
open Std

private def hist (a : Array Nat) : Nat :=
  let n := a.size
  let rec outer (i best : Nat) : Nat :=
    if i < n then
      let rec inner (j mn best : Nat) : Nat :=
        if j < n then
          let mn2 := Nat.min mn (a[j]!)
          inner (j + 1) mn2 (Nat.max best (mn2 * (j - i + 1)))
        else best
      outer (i + 1) (inner i (a[i]!) best)
    else best
  outer 0 0

private def updateHeights (s : String) (h : Array Nat) : Array Nat :=
  let chars := s.toList.toArray
  let rec go (i : Nat) (acc : Array Nat) : Array Nat :=
    if i < h.size then
      let v := if chars.getD i '0' = '1' then h[i]! + 1 else 0
      go (i + 1) (acc.push v)
    else acc
  go 0 #[]

private def zeros (n : Nat) : Array Nat :=
  let rec go (k : Nat) (acc : Array Nat) : Array Nat :=
    if k < n then go (k + 1) (acc.push 0) else acc
  go 0 #[]

partial def runCases (lines : Array String) (t tc idx : Nat) (out : List String) : List String :=
  if tc = t then out.reverse
  else
    let rc := (((lines.getD idx "").trimAscii).toString.splitOn " ").filter (· ≠ "")
    let rows := (rc.getD 0 "0").toNat!
    let cols := (rc.getD 1 "0").toNat!
    let rec step (r i : Nat) (h : Array Nat) (best : Nat) : Nat × Nat :=
      if r = rows then (best, i)
      else
        let h2 := updateHeights (lines.getD i "") h
        step (r + 1) (i + 1) h2 (Nat.max best (hist h2))
    let init := zeros cols
    let (best, nextIdx) := step 0 (idx + 1) init 0
    runCases lines t (tc + 1) nextIdx (toString best :: out)

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  let lines := ((String.splitOn data "\n").map fun s => s.replace "\r" "").toArray
  if lines.isEmpty || lines.getD 0 "" = "" then
    pure ()
  else
    IO.print (String.intercalate "\n" (runCases lines ((lines.getD 0 "0").trimAscii).toNat! 0 1 []))
