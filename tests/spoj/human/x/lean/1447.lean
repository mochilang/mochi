/- Solution for SPOJ BRCKGAME - A Game of Toy Bricks
https://www.spoj.com/problems/BRCKGAME/
-/

import Std
open Std

structure Cuboid where
  l : Nat
  w : Nat
  h : Nat

def orientations (c : Cuboid) : Array (Nat × Nat × Nat) :=
  let sort2 (a b : Nat) := if a ≥ b then (a, b) else (b, a)
  let (a1, a2) := sort2 c.l c.w
  let (b1, b2) := sort2 c.l c.h
  let (c1, c2) := sort2 c.w c.h
  #[(a1, a2, c.h), (b1, b2, c.w), (c1, c2, c.l)]

def precompute (ori : Array (Array (Nat × Nat × Nat))) (n : Nat) :
    Array (Array Nat) :=
  Id.run do
    let mut seg := Array.mkArray n (Array.mkArray n 0)
    for r in [0:n] do
      let len := r + 1
      let mut orientBest : Array (Array Nat) :=
        Array.mkArray len (Array.mkArray 3 0)
      let mut result : Array Nat := Array.mkArray len 0
      for off in [0:len] do
        let i := r - off
        let olist := ori[i]!
        let mut row := orientBest[i]!
        let mut bestVal := 0
        for o in [0:3] do
          let (L, W, H) := olist[o]!
          let mut cur := H
          for j in [i+1:r+1] do
            let rowJ := orientBest[j]!
            let olistJ := ori[j]!
            for o2 in [0:3] do
              let (L2, W2, _) := olistJ[o2]!
              if L ≥ L2 && W ≥ W2 then
                let cand := H + rowJ[o2]!
                if cand > cur then cur := cand
          row := row.set! o cur
          if cur > bestVal then bestVal := cur
        orientBest := orientBest.set! i row
        result := result.set! i bestVal
      for l in [0:len] do
        seg := seg.modify l (fun row => row.set! r (result[l]!))
    return seg

def solveCase (n m : Nat) (cubes : Array Cuboid) : Nat :=
  let ori := cubes.map orientations
  let seg := precompute ori n
  Id.run do
    let mut dp : Array (Array Nat) :=
      Array.mkArray (n+1) (Array.mkArray (m+1) 0)
    for i in [1:n+1] do
      let maxK := if i < m then i else m
      for k in [1:maxK+1] do
        let mut best := 0
        for j in [k-1:i] do
          let candidate := dp[j]![k-1]! + seg[j]![i-1]!
          if candidate > best then best := candidate
        dp := dp.modify i (fun row => row.set! k best)
    return dp[n]![m]!

partial def solveAll (toks : Array String) (idx cases : Nat)
    (acc : List String) : List String :=
  if cases = 0 then acc.reverse
  else
    let n := toks[idx]!.toNat!
    let m := toks[idx+1]!.toNat!
    let mut cubes : Array Cuboid := Array.mkArray n {l:=0,w:=0,h:=0}
    let mut j := idx + 2
    for i in [0:n] do
      let l := toks[j]!.toNat!
      let w := toks[j+1]!.toNat!
      let h := toks[j+2]!.toNat!
      cubes := cubes.set! i {l:=l,w:=w,h:=h}
      j := j + 3
    let ans := solveCase n m cubes
    solveAll toks j (cases-1) (toString ans :: acc)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (· ≠ "")
                |>.toArray
  let t := toks[0]!.toNat!
  let outs := solveAll toks 1 t []
  IO.println (String.intercalate "\n" outs)
