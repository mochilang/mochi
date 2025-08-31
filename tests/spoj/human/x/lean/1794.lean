/- Solution for SPOJ DRAGON2 - Greedy Hydra II
https://www.spoj.com/problems/DRAGON2/
-/

import Std
open Std

structure State where
  cost : Nat
  cuts : Nat

private def inf : Nat := 1000000000000

partial def dfs (u parent : Nat) (adj : Array (List (Nat × Nat))) (k : Nat) : (Array State × Nat) := Id.run do
  let mut dp : Array State := Array.mkArray (k+1) {cost := inf, cuts := 0}
  dp := dp.set! 1 {cost := 0, cuts := 0}
  let mut size : Nat := 1
  for (v,w) in adj.get! u do
    if v != parent then
      let (childDP, childSize) := dfs v u adj k
      let mut newdp : Array State := Array.mkArray (k+1) {cost := inf, cuts := 0}
      let maxS := Nat.min k size
      for s in [1:maxS+1] do
        let st := dp.get! s
        if st.cost < inf then
          -- cut this child entirely
          let nc := st.cost + w
          let nt := st.cuts + 1
          let cur := newdp.get! s
          if nc < cur.cost || (nc == cur.cost && nt < cur.cuts) then
            newdp := newdp.set! s {cost := nc, cuts := nt}
          -- include t nodes from child
          let maxT := Nat.min childSize (k - s)
          for t in [1:maxT+1] do
            let stChild := childDP.get! t
            if stChild.cost < inf then
              let nc := st.cost + stChild.cost
              let nt := st.cuts + stChild.cuts
              let cur2 := newdp.get! (s+t)
              if nc < cur2.cost || (nc == cur2.cost && nt < cur2.cuts) then
                newdp := newdp.set! (s+t) {cost := nc, cuts := nt}
      dp := newdp
      size := size + childSize
  return (dp, size)

def solve (toks : Array String) : String :=
  let n := toks.get! 0 |>.toNat!
  let m := toks.get! 1 |>.toNat!
  let k := toks.get! 2 |>.toNat!
  let mut idx := 3
  let mut adj : Array (List (Nat × Nat)) := Array.mkArray (n+1) []
  for _ in [0:n-1] do
    let a := toks.get! idx |>.toNat!
    idx := idx + 1
    let b := toks.get! idx |>.toNat!
    idx := idx + 1
    let w := toks.get! idx |>.toNat!
    idx := idx + 1
    adj := adj.modify a (fun l => (b,w) :: l)
    adj := adj.modify b (fun l => (a,w) :: l)
  let (dpRoot, _) := dfs 1 0 adj k
  let st := dpRoot.get! k
  if st.cost < inf && st.cuts + 1 == m then
    toString st.cost
  else
    "-1"

 def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                 |>.filter (fun s => s ≠ "")
                 |> Array.ofList
  IO.println (solve toks)
