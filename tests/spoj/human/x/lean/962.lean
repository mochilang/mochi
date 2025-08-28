/- Solution for SPOJ IM - Intergalactic Map
https://www.spoj.com/problems/IM/
-/

import Std
open Std

/-- Read all natural numbers from stdin. -/
def readInts : IO (Array Nat) := do
  let data ← IO.getStdin.readToEnd
  let mut arr : Array Nat := #[]
  let mut num : Nat := 0
  let mut inNum := false
  for c in data.data do
    if c ≥ 48 && c ≤ 57 then
      num := num * 10 + (c.toNat - 48)
      inNum := true
    else if inNum then
      arr := arr.push num
      num := 0
      inNum := false
  if inNum then
    arr := arr.push num
  return arr

/-- Check whether a simple path 1 -> 2 -> 3 exists. -/
def hasRoute (n : Nat) (edges : Array (Nat × Nat)) : Bool := Id.run do
  let mut adj : Array (List Nat) := Array.mkArray (n+1) []
  for (a,b) in edges do
    adj := adj.modify a (fun l => b :: l)
    adj := adj.modify b (fun l => a :: l)
  let mut color : Array Nat := Array.mkArray (n+1) 0
  let mut q : Std.Queue Nat := .empty
  for v in adj.get! 2 do
    if v != 2 && color.get! v == 0 then
      color := color.set! v v
      q := q.enqueue v
  while true do
    match q.dequeue? with
    | none => break
    | some (u, q') =>
        q := q'
        let c := color.get! u
        for v in adj.get! u do
          if v != 2 && color.get! v == 0 then
            color := color.set! v c
            q := q.enqueue v
  return color.get! 1 != 0 && color.get! 3 != 0 && color.get! 1 != color.get! 3

/-- Process all test cases from the numbers array. -/
partial def solveCases (nums : Array Nat) (idx t : Nat) (acc : Array String) : Array String :=
  if t == 0 then acc else
    let n := nums.get! idx
    let m := nums.get! (idx + 1)
    let mut edges : Array (Nat × Nat) := Array.mkEmpty m
    let mut j := idx + 2
    for _ in [0:m] do
      let a := nums.get! j
      let b := nums.get! (j + 1)
      edges := edges.push (a, b)
      j := j + 2
    let res := if hasRoute n edges then "YES" else "NO"
    solveCases nums j (t - 1) (acc.push res)

def main : IO Unit := do
  let nums ← readInts
  let t := nums.get! 0
  let outs := solveCases nums 1 t #[]
  IO.println (String.intercalate "\n" outs)
