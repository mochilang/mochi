-- https://www.spoj.com/problems/HEXAGON/

import Std.Data.Array.Basic
open Std

-- lengths of the five rows in any direction
private def rowLen : Array Int := #[3,4,5,4,3]

-- coordinates (indices of lines in x, y, z directions) for all 19 cells
private def coords : Array (Nat × Nat × Nat) :=
  #[(0,2,4), (0,3,3), (0,4,2), (1,1,4), (1,2,3), (1,3,2), (1,4,1),
    (2,0,4), (2,1,3), (2,2,2), (2,3,1), (2,4,0),
    (3,0,3), (3,1,2), (3,2,1), (3,3,0),
    (4,0,2), (4,1,1), (4,2,0)]

-- all possible assignments of numbers to the five lines in one direction
private def genAssignments (nums : Array Int) : Array (Array UInt8 × Int) := Id.run do
  let mut res : Array (Array UInt8 × Int) := Array.mkEmpty 243
  for i0 in [:3] do
  for i1 in [:3] do
  for i2 in [:3] do
  for i3 in [:3] do
  for i4 in [:3] do
    let arr : Array UInt8 := #[UInt8.ofNat i0, UInt8.ofNat i1, UInt8.ofNat i2, UInt8.ofNat i3, UInt8.ofNat i4]
    let score :=
      rowLen[0]! * nums[i0]! +
      rowLen[1]! * nums[i1]! +
      rowLen[2]! * nums[i2]! +
      rowLen[3]! * nums[i3]! +
      rowLen[4]! * nums[i4]!
    res := res.push (arr, score)
  return res

-- check that the 19 pieces implied by three assignments are all distinct
private def uniqueTriples (a b c : Array UInt8) : Bool :=
  let rec loop (i : Nat) (mask : UInt32) : Bool :=
    if h : i < coords.size then
      let (x, y, z) := coords.get ⟨i, h⟩
      let idx : UInt32 :=
        UInt32.ofNat ((a.get! x).toNat * 9 + (b.get! y).toNat * 3 + (c.get! z).toNat)
      let bit := (1 <<< idx)
      if (mask &&& bit) ≠ 0 then
        false
      else
        loop (i+1) (mask ||| bit)
    else
      true
  loop 0 0

private def solve (A B C : Array Int) : Int :=
  let assignA := genAssignments A
  let assignB := genAssignments B
  let assignC := genAssignments C
  let mut best : Int := 0
  for (aArr, sA) in assignA do
    for (bArr, sB) in assignB do
      for (cArr, sC) in assignC do
        if uniqueTriples aArr bArr cArr then
          let total := sA + sB + sC
          if total > best then
            best := total
  best

private def readInts : IO (Array Int) := do
  let data ← IO.readStdin
  let mut arr : Array Int := #[]
  for token in data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') do
    if token ≠ "" then
      match token.toInt? with
      | some v => arr := arr.push v
      | none   => pure ()
  return arr

def main : IO Unit := do
  let nums ← readInts
  let n := nums[0]!.toNat
  let mut idx := 1
  for caseIdx in [1:n+1] do
    let A := #[nums[idx]!, nums[idx+1]!, nums[idx+2]!]; idx := idx + 3
    let B := #[nums[idx]!, nums[idx+1]!, nums[idx+2]!]; idx := idx + 3
    let C := #[nums[idx]!, nums[idx+1]!, nums[idx+2]!]; idx := idx + 3
    let ans := solve A B C
    IO.println s!"Test #{caseIdx}"
    IO.println ans
    IO.println ""
