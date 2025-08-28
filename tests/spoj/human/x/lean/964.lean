/- Solution for SPOJ EN - Entrapment
https://www.spoj.com/problems/EN/
-/

import Std
open Std

partial def dfsRPO (n : Nat) (succ : Array (Array Nat)) : Array Nat :=
  Id.run do
    let mut visited := Array.mkArray (n+1) false
    let mut stack : List (Nat × Bool) := [(1, false)]
    let mut order : Array Nat := #[]
    while let (u,flag) :: stackRest := stack do
      stack := stackRest
      if !flag then
        if visited.get! u then
          pure ()
        else
          visited := visited.set! u true
          stack := (u, true) :: stack
          for v in succ.get! u do
            stack := (v, false) :: stack
      else
        order := order.push u
    return order.reverse

partial def intersect (rpos : Array Nat) (idom : Array Nat) (a b : Nat) : Nat :=
  Id.run do
    let mut x := a
    let mut y := b
    while x ≠ y do
      while rpos.get! x > rpos.get! y do
        x := idom.get! x
      while rpos.get! y > rpos.get! x do
        y := idom.get! y
    return x

partial def dominators (n : Nat) (succ pred : Array (Array Nat)) : Array Nat :=
  Id.run do
    let rpo := dfsRPO n succ
    let size := rpo.size
    let mut rpos : Array Nat := Array.mkArray (n+1) 0
    for i in [0:size] do
      let v := rpo.get! i
      rpos := rpos.set! v i
    let mut idom : Array Nat := Array.mkArray (n+1) 0
    idom := idom.set! 1 1
    let mut changed := true
    while changed do
      changed := false
      for i in [1:size] do
        let v := rpo.get! i
        let preds := pred.get! v
        let mut newI := 0
        for p in preds do
          if idom.get! p ≠ 0 then
            newI := if newI == 0 then p else intersect rpos idom p newI
        if idom.get! v ≠ newI then
          idom := idom.set! v newI
          changed := true
    return idom

def solve (toks : Array String) : Array String :=
  Id.run do
    let t := toks.get! 0 |>.toNat!
    let mut idx := 1
    let mut outs : Array String := #[]
    for _ in [0:t] do
      let n := toks.get! idx |>.toNat!
      let m := toks.get! (idx+1) |>.toNat!
      idx := idx + 2
      let mut succ : Array (Array Nat) := Array.mkArray (n+1) #[]
      let mut pred : Array (Array Nat) := Array.mkArray (n+1) #[]
      for _ in [0:m] do
        let a := toks.get! idx |>.toNat!
        let b := toks.get! (idx+1) |>.toNat!
        idx := idx + 2
        succ := succ.modify a (fun arr => arr.push b)
        pred := pred.modify b (fun arr => arr.push a)
      let idom := dominators n succ pred
      let mut x := n
      while idom.get! x ≠ 1 do
        x := idom.get! x
      outs := outs.push (toString x)
    return outs

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "")
                |> Array.ofList
  let res := solve toks
  for line in res do
    IO.println line
