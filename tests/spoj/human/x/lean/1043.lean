/- Solution for SPOJ GSS1 - Can you answer these queries I
https://www.spoj.com/problems/GSS1/
-/

import Std
open Std

structure Node where
  sum pref suff best : Int

def combine (a b : Node) : Node :=
  let sum := a.sum + b.sum
  let pref := max a.pref (a.sum + b.pref)
  let suff := max b.suff (b.sum + a.suff)
  let best := max (max a.best b.best) (a.suff + b.pref)
  {sum := sum, pref := pref, suff := suff, best := best}

def mkNode (v : Int) : Node := {sum := v, pref := v, suff := v, best := v}

def negInf : Int := -1000000000000000

def nullNode : Node := {sum := 0, pref := negInf, suff := negInf, best := negInf}

partial def build (arr : Array Int) (tree : Array Node) (idx l r : Nat) : Array Node :=
  if h : l = r then
    let v := arr.get! l
    tree.set! idx (mkNode v)
  else
    let mid := (l + r) / 2
    let tree := build arr tree (idx * 2) l mid
    let tree := build arr tree (idx * 2 + 1) (mid + 1) r
    let left := tree.get! (idx * 2)
    let right := tree.get! (idx * 2 + 1)
    tree.set! idx (combine left right)
termination_by _ => r - l

partial def query (tree : Array Node) (idx l r ql qr : Nat) : Node :=
  if qr < l || r < ql then
    nullNode
  else if ql ≤ l && r ≤ qr then
    tree.get! idx
  else
    let mid := (l + r) / 2
    let left := query tree (idx * 2) l mid ql qr
    let right := query tree (idx * 2 + 1) (mid + 1) r ql qr
    combine left right
termination_by _ => r - l

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (· ≠ "")
                |> List.toArray
  if toks.size = 0 then
    return ()
  else
    let mut i := 0
    let n := toks[i]!.toNat!
    i := i + 1
    let mut arr : Array Int := Array.mkArray n 0
    for j in [0:n] do
      arr := arr.set! j (toks[i]!.toInt!)
      i := i + 1
    let m := toks[i]!.toNat!
    i := i + 1
    let mut tree : Array Node := Array.mkArray (4 * n) nullNode
    let tree := build arr tree 1 0 (n - 1)
    for _ in [0:m] do
      let x := toks[i]!.toNat! - 1
      let y := toks[i+1]!.toNat! - 1
      i := i + 2
      let res := query tree 1 0 (n - 1) x y
      IO.println res.best
