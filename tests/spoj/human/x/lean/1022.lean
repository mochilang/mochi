/- Solution for SPOJ ANGELS - Angels and Devils
https://www.spoj.com/problems/ANGELS/
-/

import Std
open Std

def readTokens : IO (Array String) := do
  let data ← IO.readStdin
  return data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (fun s => s ≠ "")

partial def augment (u : Nat) (adj : Array (List Nat))
    (matchR : Array (Option Nat)) (vis : Array Bool) : Option (Array (Option Nat)) :=
  let rec go (edges : List Nat) (matchR : Array (Option Nat)) (vis : Array Bool) :=
    match edges with
    | [] => none
    | v :: vs =>
        if vis.get! v then
          go vs matchR vis
        else
          let vis := vis.set! v true
          match matchR.get! v with
          | none => some (matchR.set! v (some u))
          | some u2 =>
              match augment u2 adj matchR vis with
              | some m2 => some (m2.set! v (some u))
              | none    => go vs matchR vis
  go (adj.get! u) matchR vis

def maxMatching (adj : Array (List Nat)) (rsize : Nat) : Nat := Id.run do
  let mut matchR : Array (Option Nat) := Array.mkArray rsize none
  let mut res := 0
  for u in [0:adj.size] do
    let vis := Array.mkArray rsize false
    match augment u adj matchR vis with
    | some m2 =>
        matchR := m2
        res := res + 1
    | none => pure ()
  return res

partial def solve (tokens : Array String) (idx : Nat) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let x := (tokens.get! idx).toNat!
    let y := (tokens.get! (idx + 1)).toNat!
    let mut idx := idx + 2
    let mut grid : Array (Array Bool) := Array.mkArray x (Array.mkArray y false)
    for i in [0:x] do
      let mut row := grid.get! i
      for j in [0:y] do
        let ch := tokens.get! idx; idx := idx + 1
        row := row.set! j (ch = "H")
      grid := grid.set! i row
    -- horizontal segments
    let mut rowId : Array (Array Nat) := Array.mkArray x (Array.mkArray y 0)
    let mut hCnt := 0
    for i in [0:x] do
      let row := grid.get! i
      let mut rid := rowId.get! i
      let mut j := 0
      while j < y do
        if row.get! j then
          let id := hCnt
          hCnt := hCnt + 1
          let mut k := j
          while k < y && row.get! k do
            rid := rid.set! k id
            k := k + 1
          j := k
        else
          j := j + 1
      rowId := rowId.set! i rid
    -- vertical segments
    let mut colId : Array (Array Nat) := Array.mkArray x (Array.mkArray y 0)
    let mut vCnt := 0
    for j in [0:y] do
      let mut i := 0
      while i < x do
        if (grid.get! i).get! j then
          let id := vCnt
          vCnt := vCnt + 1
          let mut k := i
          while k < x && (grid.get! k).get! j do
            let mut colRow := colId.get! k
            colRow := colRow.set! j id
            colId := colId.set! k colRow
            k := k + 1
          i := k
        else
          i := i + 1
    -- build bipartite graph
    let mut adj : Array (List Nat) := Array.replicate hCnt []
    for i in [0:x] do
      let row := grid.get! i
      let rid := rowId.get! i
      let cid := colId.get! i
      for j in [0:y] do
        if row.get! j then
          let r := rid.get! j
          let c := cid.get! j
          adj := adj.modify r (fun lst => c :: lst)
    let ans := maxMatching adj vCnt
    IO.println (toString ans)
    solve tokens idx (t - 1)

def main : IO Unit := do
  let tokens ← readTokens
  let t := (tokens.get! 0).toNat!
  solve tokens 1 t
