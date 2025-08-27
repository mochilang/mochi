/- Solution for SPOJ ALIBB - Alibaba
https://www.spoj.com/problems/ALIBB/
-/
import Std
open Std

def solveCase (pairs : Array (Int × Int)) : Option Int := Id.run do
  let n := pairs.size
  if n = 0 then
    return some 0
  let d1 := pairs.get! 0 |>.fst
  let dn := pairs.get! (n-1) |>.fst
  let span := dn - d1
  let big : Int := 1000000000000000
  -- prefix minimum of d_i + t_i
  let mut pre : Array Int := Array.mkArray n big
  let mut m := big
  for i in [0:n] do
    let di := pairs.get! i |>.fst
    let ti := pairs.get! i |>.snd
    m := min m (di + ti)
    pre := pre.set! i m
  -- suffix minimum of t_j + 2*d1 - d_j for j >= i+1
  let mut suf : Array Int := Array.mkArray n big
  let mut m2 := big
  for ii in [0:n] do
    let i := n - 1 - ii
    if i + 1 < n then
      let dj := pairs.get! (i+1) |>.fst
      let tj := pairs.get! (i+1) |>.snd
      m2 := min m2 (tj + (2*d1 - dj))
    else
      m2 := big
    suf := suf.set! i m2
  let mut best : Option Int := none
  -- try going left first
  let mut s : Nat := 0
  while s < n && best.isNone do
    let ds := pairs.get! s |>.fst
    let bound := min (pre.get! s) (suf.get! s)
    if ds <= bound then
      best := some (span + (ds - d1))
    s := s + 1
  -- suffix max of d_j - t_j
  let mut sufMax : Array Int := Array.mkArray n (-big)
  let mut m3 := -big
  for ii in [0:n] do
    let i := n - 1 - ii
    let dj := pairs.get! i |>.fst
    let tj := pairs.get! i |>.snd
    m3 := max m3 (dj - tj)
    sufMax := sufMax.set! i m3
  -- prefix max of 2*dn - d_i - t_i
  let mut preMax : Array Int := Array.mkArray (n+1) (-big)
  let mut m4 := -big
  for i in [0:n] do
    let di := pairs.get! i |>.fst
    let ti := pairs.get! i |>.snd
    m4 := max m4 (2*dn - di - ti)
    preMax := preMax.set! (i+1) m4
  -- try going right first
  let mut idx : Nat := n
  while idx > 0 && best.isNone do
    let s := idx - 1
    let ds := pairs.get! s |>.fst
    let bound := max (preMax.get! s) (sufMax.get! s)
    if ds >= bound then
      let t := span + (dn - ds)
      best := match best with
              | some cur => some (min cur t)
              | none => some t
      break
    idx := s
  return best

partial def solveAll (toks : Array String) (i : Nat) (k : Nat) (acc : List String) : List String :=
  if k = 0 then acc.reverse else
    let n := toks.get! i |>.toNat!
    let mut pairs : Array (Int × Int) := Array.mkArray n (0,0)
    for j in [0:n] do
      let d := Int.ofNat (toks.get! (i + 1 + 2*j) |>.toNat!)
      let t := Int.ofNat (toks.get! (i + 1 + 2*j + 1) |>.toNat!)
      pairs := pairs.set! j (d,t)
    let res := match solveCase pairs with
      | some v => toString v
      | none => "No solution"
    solveAll toks (i + 1 + 2*n) (k-1) (res :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
               |>.filter (fun s => s ≠ "")
               |> Array.ofList
  let k := toks.get! 0 |>.toNat!
  let res := solveAll toks 1 k []
  for s in res do
    IO.println s
