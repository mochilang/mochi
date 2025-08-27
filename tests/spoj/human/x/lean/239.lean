/- Solution for SPOJ BTOUR - Tour de Byteland
https://www.spoj.com/problems/BTOUR/
-/

import Std
open Std

structure State where
  best : Nat
  count : Nat

partial def dfs (start curr len : Nat) (visited : List Nat)
    (adj : Array (List (Nat × Nat))) (ref : IO.Ref State) : IO Unit := do
  for (next, w) in adj.get! curr do
    let st ← ref.get
    let newLen := len + w
    if newLen ≤ st.best then
      if next == start then
        if visited.length ≥ 3 then
          if newLen < st.best then
            ref.set {best := newLen, count := 1}
          else if newLen == st.best then
            ref.modify fun s => {s with count := s.count + 1}
        else
          pure ()
      else if next > start && !(visited.contains next) then
        dfs start next newLen (next :: visited) adj ref

partial def solveCase (n : Nat) (adj : Array (List (Nat × Nat))) : IO (Nat × Nat) := do
  let ref ← IO.mkRef {best := (1 <<< 60), count := 0} -- large number as infinity
  for start in [1:n+1] do
    dfs start start 0 [start] adj ref
  let st ← ref.get
  if st.count == 0 then
    pure (0,0)
  else
    pure (st.best, st.count)

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let parts := (← h.getLine).trim.split (· = ' ') |>.filter (· ≠ "")
    let n := parts[0]!.toNat!
    let m := parts[1]!.toNat!
    let mut adj : Array (List (Nat × Nat)) := Array.mkArray (n+1) []
    for _ in [0:m] do
      let ps := (← h.getLine).trim.split (· = ' ') |>.filter (· ≠ "")
      let u := ps[0]!.toNat!
      let v := ps[1]!.toNat!
      let d := ps[2]!.toNat!
      adj := adj.set! u ((v,d)::adj.get! u)
      adj := adj.set! v ((u,d)::adj.get! v)
    let (best,count) ← solveCase n adj
    IO.println s!"{best} {count}"
    process h (t-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
