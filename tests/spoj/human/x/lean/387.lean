/- Solution for SPOJ TOURS - Travelling tours
https://www.spoj.com/problems/TOURS/
-/

import Std
open Std

private def inf : Int := 1000000000

partial def hungarian (cost : Array (Array Int)) : Int :=
  let n := cost.size
  Id.run do
    let mut u : Array Int := Array.mkArray (n+1) 0
    let mut v : Array Int := Array.mkArray (n+1) 0
    let mut p : Array Nat := Array.mkArray (n+1) 0
    let mut way : Array Nat := Array.mkArray (n+1) 0
    for i in [1:n+1] do
      p := p.set! 0 i
      let mut j0 : Nat := 0
      let mut minv : Array Int := Array.mkArray (n+1) inf
      minv := minv.set! 0 0
      let mut used : Array Bool := Array.mkArray (n+1) false
      while true do
        used := used.set! j0 true
        let i0 := p.get! j0
        let mut delta : Int := inf
        let mut j1 : Nat := 0
        for j in [1:n+1] do
          if !(used.get! j) then
            let cur := (cost.get! (i0-1)).get! (j-1) - u.get! i0 - v.get! j
            if cur < minv.get! j then
              minv := minv.set! j cur
              way := way.set! j j0
            if minv.get! j < delta then
              delta := minv.get! j
              j1 := j
        for j in [0:n+1] do
          if used.get! j then
            u := u.set! (p.get! j) (u.get! (p.get! j) + delta)
            v := v.set! j (v.get! j - delta)
          else
            minv := minv.set! j (minv.get! j - delta)
        j0 := j1
        if p.get! j0 == 0 then
          break
      while true do
        let j1 := way.get! j0
        p := p.set! j0 (p.get! j1)
        j0 := j1
        if j0 == 0 then
          break
    let mut match : Array Nat := Array.mkArray (n+1) 0
    for j in [1:n+1] do
      match := match.set! (p.get! j) j
    let mut ans : Int := 0
    for i in [1:n+1] do
      ans := ans + (cost.get! (i-1)).get! (match.get! i - 1)
    return ans

partial def solve (tokens : Array String) (idx t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let n := (tokens.get! idx).toNat!
    let m := (tokens.get! (idx+1)).toNat!
    let mut idx := idx + 2
    let mut cost : Array (Array Int) := Array.mkArray n (Array.mkArray n inf)
    for _ in [0:m] do
      let u := (tokens.get! idx).toNat! - 1
      let v := (tokens.get! (idx+1)).toNat! - 1
      let w := (tokens.get! (idx+2)).toNat!
      idx := idx + 3
      if u ≠ v then
        cost := cost.modify u (fun row =>
          let old := row.get! v
          if (Int.ofNat w) < old then row.set! v (Int.ofNat w) else row)
    IO.println (hungarian cost)
    solve tokens idx (t - 1)

def main : IO Unit := do
  let data ← IO.readStdin
  let tokens := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "")
                |> Array.ofList
  let t := (tokens.get! 0).toNat!
  solve tokens 1 t
