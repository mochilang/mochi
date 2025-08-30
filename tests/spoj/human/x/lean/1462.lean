/- Solution for SPOJ BARB - Barbarians
https://www.spoj.com/problems/BARB/
-/
import Std
open Std

structure Barb where
  c : Nat
  p : Nat
  l : Nat
deriving Inhabited

partial def egcd (a b : Int) : (Int × Int × Int) :=
  if a == 0 then (b, 0, 1)
  else
    let (g, x1, y1) := egcd (b % a) a
    (g, y1 - (b / a) * x1, x1)

def modInv (a m : Int) : Int :=
  let (g, x, _) := egcd a m
  if g != 1 then 0 else
    let x := x % m
    if x < 0 then x + m else x

def conflict (b1 b2 : Barb) (mNat : Nat) : Bool :=
  let m := Int.ofNat mNat
  let limit := Int.ofNat (min b1.l b2.l)
    let c1 := Int.ofNat b1.c
    let c2 := Int.ofNat b2.c
    let p1 := Int.ofNat b1.p
    let p2 := Int.ofNat b2.p
  let a := p1 - p2
  let b := c2 - c1
  let g := Int.gcd a m
  if b % g != 0 then false else
    let m1 := m / g
    let a1 := (a / g) % m1
    let b1' := b / g
    let inv := modInv a1 m1
    let t := (b1' * inv) % m1
    t >= 0 && t ≤ limit

partial def validM (arr : Array Barb) (m : Nat) : Bool :=
  let n := arr.size
  let rec outer (i : Nat) : Bool :=
    if i >= n then true else
      let rec inner (j : Nat) : Bool :=
        if j >= n then true else
          if conflict arr[i]! arr[j]! m then false
          else inner (j+1)
      if inner (i+1) then outer (i+1) else false
  outer 0

partial def findM (arr : Array Barb) : Nat :=
  let start := arr.foldl (fun acc b => max acc b.c) 1
  let rec loop (m : Nat) : Nat :=
    if validM arr m then m else loop (m+1)
  loop start

partial def readBarbs (toks : Array String) (idx n : Nat) (acc : Array Barb)
    : (Array Barb × Nat) :=
  if n == 0 then (acc, idx) else
    let c := toks[idx]!.toNat!
    let p := toks[idx+1]!.toNat!
    let l := toks[idx+2]!.toNat!
    readBarbs toks (idx+3) (n-1) (acc.push {c := c, p := p, l := l})

partial def solveAll (toks : Array String) (idx t : Nat) (acc : List Nat)
    : (List Nat) :=
  if t == 0 then acc.reverse else
    let n := toks[idx]!.toNat!
    let (bs, nxt) := readBarbs toks (idx+1) n #[]
    let m := findM bs
    solveAll toks nxt (t-1) (m :: acc)

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |> List.toArray
  let t := toks[0]!.toNat!
  let outs := solveAll toks 1 t []
  for o in outs do
    IO.println o
