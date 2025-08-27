/- Solution for SPOJ SCUBADIV - Scuba diver
https://www.spoj.com/problems/SCUBADIV/
-/

import Std
open Std

def INF : Nat := 1000000000

def solveCase (reqO reqN : Nat) (cyls : Array (Nat × Nat × Nat)) : Nat := Id.run do
  let t := reqO
  let a := reqN
  let mut dp : Array (Array Nat) := Array.mkArray (t+1) (Array.mkArray (a+1) INF)
  let row0 := (dp.get! 0).set! 0 0
  dp := dp.set! 0 row0
  for cyl in cyls do
    let (o, n, w) := cyl
    let mut ndp := dp
    for i in [0:t+1] do
      let row := dp.get! i
      for j in [0:a+1] do
        let prev := row.get! j
        if prev != INF then
          let ni := min t (i + o)
          let nj := min a (j + n)
          let old := (ndp.get! ni).get! nj
          let cand := prev + w
          if cand < old then
            let arr := (ndp.get! ni).set! nj cand
            ndp := ndp.set! ni arr
    dp := ndp
  (dp.get! t).get! a

partial def solveAll (toks : Array String) (idx t : Nat) (acc : List Nat) : List Nat :=
  if t = 0 then acc.reverse else
    let reqO := toks.get! idx |>.toNat!
    let reqN := toks.get! (idx+1) |>.toNat!
    let n := toks.get! (idx+2) |>.toNat!
    let mut cyls : Array (Nat × Nat × Nat) := Array.mkArray n (0,0,0)
    let mut i := idx + 3
    for j in [0:n] do
      let o := toks.get! i |>.toNat!
      let nn := toks.get! (i+1) |>.toNat!
      let w := toks.get! (i+2) |>.toNat!
      cyls := cyls.set! j (o, nn, w)
      i := i + 3
    let res := solveCase reqO reqN cyls
    solveAll toks i (t-1) (res :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
               |>.filter (fun s => s ≠ "")
               |> Array.ofList
  let T := toks.get! 0 |>.toNat!
  let outs := solveAll toks 1 T []
  for v in outs do
    IO.println v
