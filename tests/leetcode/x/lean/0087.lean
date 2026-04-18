import Std
open Std

private def sameSlice (a b : Array Char) (i1 i2 len : Nat) : Bool :=
  let rec go (k : Nat) : Bool :=
    if k < len then
      if a.getD (i1 + k) ' ' = b.getD (i2 + k) ' ' then go (k + 1) else false
    else
      true
  go 0

private def sameLetters (a b : Array Char) (i1 i2 len : Nat) : Bool :=
  let rec addA (k : Nat) (cnt : Array Int) : Array Int :=
    if k < len then
      let idx := (a.getD (i1 + k) 'a').toNat - 'a'.toNat;
      let cur := cnt.getD idx 0;
      addA (k + 1) (cnt.set! idx (cur + 1))
    else cnt
  let rec addB (k : Nat) (cnt : Array Int) : Array Int :=
    if k < len then
      let idx := (b.getD (i2 + k) 'a').toNat - 'a'.toNat;
      let cur := cnt.getD idx 0;
      addB (k + 1) (cnt.set! idx (cur - 1))
    else cnt
  let cnt0 := Array.replicate 26 0
  let cnt := addB 0 (addA 0 cnt0)
  let rec check (k : Nat) : Bool := if k < 26 then cnt.getD k 0 = 0 && check (k + 1) else true
  check 0

partial def dfsScramble (a1 a2 : Array Char) (i1 i2 len : Nat) : Bool :=
  if sameSlice a1 a2 i1 i2 len then true
  else if !(sameLetters a1 a2 i1 i2 len) then false
  else
    let rec loop (k : Nat) : Bool :=
      if k >= len then false
      else if (dfsScramble a1 a2 i1 i2 k) && (dfsScramble a1 a2 (i1 + k) (i2 + k) (len - k)) then true
      else if (dfsScramble a1 a2 i1 (i2 + len - k) k) && (dfsScramble a1 a2 (i1 + k) i2 (len - k)) then true
      else loop (k + 1)
    loop 1

private def solve (s1 s2 : String) : Bool :=
  dfsScramble s1.toList.toArray s2.toList.toArray 0 0 s1.length

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  let lines := ((String.splitOn data "\n").map fun s => s.replace "\r" "").toArray
  if lines.isEmpty || lines.getD 0 "" = "" then pure () else
    let t := ((lines.getD 0 "0").trimAscii).toNat!
    let mut out : Array String := #[]
    for i in List.range t do
      out := out.push (if solve (lines.getD (1 + 2 * i) "") (lines.getD (2 + 2 * i) "") then "true" else "false")
    IO.print (String.intercalate "\n" out.toList)
