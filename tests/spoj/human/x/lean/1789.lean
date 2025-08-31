/- Solution for SPOJ GREEDULM - Huffman's Greed
https://www.spoj.com/problems/GREEDULM/
-/

import Std
open Std

/-- Compute minimal cost*s for optimal BST given key and gap frequencies. -/
private def optimal (p : Array Nat) (q : Array Nat) : Nat :=
  let n := p.size
  -- dp arrays: e and w of size (n+1) x (n+1)
  Id.run do
    let mut e : Array (Array Nat) := Array.replicate (n+1) (Array.replicate (n+1) 0)
    let mut w : Array (Array Nat) := Array.replicate (n+1) (Array.replicate (n+1) 0)
    for i in [0:n+1] do
      let wi := (w[i]!).set! i (q[i]!)
      w := w.set! i wi
    for len in [1:n+1] do
      for i in [0:(n - len + 1)] do
        let j := i + len
        let wi := w[i]!
        let wj := wi[j - 1]! + p[j - 1]! + q[j]!
        let wi := wi.set! j wj
        w := w.set! i wi
        let mut best : Nat := 1000000000000 -- big number
        for r in [i:j] do
          let cost := e[i]![r]! + e[r + 1]![j]! + wj
          if cost < best then best := cost
        let ei := (e[i]!).set! j best
        e := e.set! i ei
    return e[0]![n]!

/-- Process tokens of all test cases and return output lines. -/
partial def process (vals : Array Nat) (idx : Nat) (acc : Array String) : Array String :=
  if idx < vals.size then
    let n := vals[idx]!
    if n = 0 then acc else
      let cost := Id.run do
        let mut p : Array Nat := Array.replicate n 0
        for i in [0:n] do
          p := p.set! i (vals[idx + 1 + i]!)
        let mut q : Array Nat := Array.replicate (n+1) 0
        for i in [0:n+1] do
          q := q.set! i (vals[idx + 1 + n + i]!)
        return optimal p q
      process vals (idx + 1 + n + (n+1)) (acc.push s!"{cost}")
  else acc

/-- Read all numbers from stdin and print minimal costs for each case. -/
def main : IO Unit := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
  let vals := toks.map (·.toNat!) |>.toArray
  let outs := process vals 0 #[]
  for s in outs do
    IO.println s
