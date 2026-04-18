import Std
open Std

private def parseNat (s : String) : Nat :=
  s.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0

private def solveArr (a : Array Nat) : Nat :=
  let n := a.size
  let rec outer (i best : Nat) : Nat :=
    if i < n then
      let rec inner (j mn best : Nat) : Nat :=
        if j < n then
          let mn2 := Nat.min mn (a[j]!)
          let area := mn2 * (j - i + 1)
          inner (j + 1) mn2 (Nat.max best area)
        else
          best
      outer (i + 1) (inner i (a[i]!) best)
    else
      best
  outer 0 0

partial def runCases (lines : Array String) (t tc idx : Nat) (out : List String) : List String :=
  if tc = t then out.reverse
  else
    let n := ((lines.getD idx "0").trimAscii).toNat!
    let rec build (k : Nat) (acc : Array Nat) : Array Nat :=
      if k < n then build (k + 1) (acc.push (((lines.getD (idx + 1 + k) "0").trimAscii).toNat!)) else acc
    let arr := build 0 #[]
    runCases lines t (tc + 1) (idx + n + 1) (toString (solveArr arr) :: out)

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  let lines := ((String.splitOn data "\n").map fun s => s.replace "\r" "").toArray
  if lines.isEmpty || lines.getD 0 "" = "" then
    pure ()
  else
    let t := ((lines.getD 0 "0").trimAscii).toNat!
    IO.print (String.intercalate "\n" (runCases lines t 0 1 []))
