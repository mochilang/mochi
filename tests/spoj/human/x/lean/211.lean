/- Solution for SPOJ PRIMIT - Primitivus recurencis
https://www.spoj.com/problems/PRIMIT/
-/

import Std
open Std

structure DSU where
  parent : IO.Ref (Array Nat)

namespace DSU

partial def find (d : DSU) (x : Nat) : IO Nat := do
  let pArr ← d.parent.get
  let p := pArr[x]!
  if p = x then
    pure x
  else
    let r ← find d p
    let pArr ← d.parent.get
    d.parent.set (pArr.set! x r)
    pure r

def union (d : DSU) (a b : Nat) : IO Unit := do
  let ra ← find d a
  let rb ← find d b
  if ra ≠ rb then
    let pArr ← d.parent.get
    d.parent.set (pArr.set! ra rb)

def init (n : Nat) : IO DSU := do
  let parent ← IO.mkRef (List.range n |>.toArray)
  pure ⟨parent⟩

end DSU

def solveCase (n : Nat) (pairs : Array (Nat × Nat)) : IO Nat := do
  let sz := 1001
  let dsu ← DSU.init sz
  let mut indeg := Array.replicate sz 0
  let mut outdeg := Array.replicate sz 0
  for (a, b) in pairs do
    outdeg := outdeg.set! a (outdeg[a]! + 1)
    indeg := indeg.set! b (indeg[b]! + 1)
    DSU.union dsu a b
  let mut compP := Array.replicate sz 0
  let mut compE := Array.replicate sz 0
  for v in [0:sz] do
    let e := outdeg[v]!
    let i := indeg[v]!
    if e + i > 0 then
      let r ← DSU.find dsu v
      compE := compE.set! r (compE[r]! + e)
      let diff := if e > i then e - i else 0
      compP := compP.set! r (compP[r]! + diff)
  let mut sumExtra := 0
  let mut c := 0
  for r in [0:sz] do
    let e := compE[r]!
    if e > 0 then
      let p := compP[r]!
      let extra := if p > 0 then p - 1 else 0
      sumExtra := sumExtra + extra
      c := c + 1
  return n + sumExtra + c

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (· ≠ "") |> List.toArray
  if toks.size = 0 then
    pure ()
  else
    let t := String.toNat! (toks[0]!)
    let mut idx := 1
    for _ in [0:t] do
      let m := String.toNat! (toks[idx]!)
      idx := idx + 1
      let mut pairs : Array (Nat × Nat) := Array.mkEmpty m
      for _ in [0:m] do
        let a := String.toNat! (toks[idx]!)
        let b := String.toNat! (toks[idx + 1]!)
        idx := idx + 2
        pairs := pairs.push (a, b)
      let ans ← solveCase m pairs
      IO.println ans
