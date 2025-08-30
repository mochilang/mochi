/- Solution for SPOJ GSS3 - Can you answer these queries III
https://www.spoj.com/problems/GSS3/
-/

import Std
open Std

structure Node where
  sum : Int
  pref : Int
  suff : Int
  best : Int

def negInf : Int := -1000000000000000000

def mkLeaf (v : Int) : Node :=
  {sum := v, pref := v, suff := v, best := v}

def combine (l r : Node) : Node :=
  { sum := l.sum + r.sum
  , pref := max l.pref (l.sum + r.pref)
  , suff := max r.suff (r.sum + l.suff)
  , best := max (max l.best r.best) (l.suff + r.pref) }

partial def build (arr : Array Int) (treeRef : IO.Ref (Array Node))
    (idx l r : Nat) : IO Unit := do
  if l == r then
    let node := mkLeaf arr[l]!
    treeRef.modify (fun t => t.set! idx node)
  else
    let mid := (l + r) / 2
    build arr treeRef (idx * 2) l mid
    build arr treeRef (idx * 2 + 1) (mid + 1) r
    let t ← treeRef.get
    let node := combine t[idx * 2]! t[idx * 2 + 1]!
    treeRef.modify (fun t => t.set! idx node)

partial def update (treeRef : IO.Ref (Array Node))
    (idx l r pos : Nat) (v : Int) : IO Unit := do
  if l == r then
    treeRef.modify (fun t => t.set! idx (mkLeaf v))
  else
    let mid := (l + r) / 2
    if pos ≤ mid then
      update treeRef (idx * 2) l mid pos v
    else
      update treeRef (idx * 2 + 1) (mid + 1) r pos v
    let t ← treeRef.get
    let node := combine t[idx * 2]! t[idx * 2 + 1]!
    treeRef.modify (fun t => t.set! idx node)

partial def query (treeRef : IO.Ref (Array Node))
    (idx l r ql qr : Nat) : IO Node := do
  if ql ≤ l ∧ r ≤ qr then
    return (← treeRef.get)[idx]!
  else
    let mid := (l + r) / 2
    if qr ≤ mid then
      query treeRef (idx * 2) l mid ql qr
    else if ql > mid then
      query treeRef (idx * 2 + 1) (mid + 1) r ql qr
    else
      let left ← query treeRef (idx * 2) l mid ql qr
      let right ← query treeRef (idx * 2 + 1) (mid + 1) r ql qr
      return combine left right

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
              |>.filter (· ≠ "") |> List.toArray
  if toks.size = 0 then
    return ()
  let mut idx : Nat := 0
  let n := (toks[idx]!).toNat!
  idx := idx + 1
  let mut arr : Array Int := Array.mkArray n 0
  for i in [0:n] do
    arr := arr.set! i (toks[idx]!).toInt!
    idx := idx + 1
  let m := (toks[idx]!).toNat!
  idx := idx + 1
  let default := {sum := 0, pref := negInf, suff := negInf, best := negInf}
  let treeInit : Array Node := Array.mkArray (4 * n) default
  let treeRef ← IO.mkRef treeInit
  if n > 0 then
    build arr treeRef 1 0 (n - 1)
  let mut outputs : Array String := #[]
  for _ in [0:m] do
    let typ := (toks[idx]!).toNat!
    idx := idx + 1
    let x := (toks[idx]!).toNat!
    idx := idx + 1
    if typ == 0 then
      let v := (toks[idx]!).toInt!
      idx := idx + 1
      update treeRef 1 0 (n - 1) (x - 1) v
    else
      let y := (toks[idx]!).toNat!
      idx := idx + 1
      let res ← query treeRef 1 0 (n - 1) (x - 1) (y - 1)
      outputs := outputs.push (toString res.best)
  IO.println (String.intercalate "\n" outputs.toList)
