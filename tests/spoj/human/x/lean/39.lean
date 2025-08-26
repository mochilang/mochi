/- Solution for SPOJ PIGBANK - Piggy-Bank
https://www.spoj.com/problems/PIGBANK/
-/
import Std
open Std

def INF : Nat := 1000000000

def solveCase (diff : Nat) (coins : Array (Nat × Nat)) : Option Nat := Id.run do
  let mut dp := Array.mkArray (diff + 1) INF
  dp := dp.set! 0 0
  for w in [1:diff+1] do
    let mut best := INF
    for coin in coins do
      let value := coin.fst
      let weight := coin.snd
      if weight ≤ w then
        let prev := dp.get! (w - weight)
        let cand := prev + value
        if cand < best then
          best := cand
    dp := dp.set! w best
  let ans := dp.get! diff
  if ans == INF then none else some ans

partial def solveAll (toks : Array String) (idx t : Nat) (acc : List String) : List String :=
  if t = 0 then acc.reverse else
    let E := toks.get! idx |>.toNat!
    let F := toks.get! (idx+1) |>.toNat!
    let diff := F - E
    let N := toks.get! (idx+2) |>.toNat!
    let mut coins : Array (Nat × Nat) := Array.mkArray N (0,0)
    let mut i := idx + 3
    for j in [0:N] do
      let p := toks.get! i |>.toNat!
      let w := toks.get! (i+1) |>.toNat!
      coins := coins.set! j (p, w)
      i := i + 2
    let res := solveCase diff coins
    let line := match res with
      | some v => s!"The minimum amount of money in the piggy-bank is {v}."
      | none => "This is impossible."
    solveAll toks i (t-1) (line :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
               |>.filter (fun s => s ≠ "")
               |> Array.ofList
  let T := toks.get! 0 |>.toNat!
  let outs := solveAll toks 1 T []
  for line in outs do
    IO.println line
