/- Solution for SPOJ BUS - Bus
https://www.spoj.com/problems/BUS/
-/
import Std
open Std

def lcmNat (a b : Nat) : Nat :=
  if a == 0 then b
  else if b == 0 then a
  else (a / Nat.gcd a b) * b

partial def simulate (routes : Array (Array Nat)) : String :=
  let n := routes.size
  let lens := routes.map fun r => r.size
  let period := lens.foldl lcmNat 1
  let target := (1 <<< n) - 1
  let initKnow := (Array.range n).map fun i => (1 <<< i)
  let rec loop (t : Nat) (know : Array Nat) : Option Nat :=
    if t > period then
      none
    else
      let pos := (Array.range n).map fun i =>
        let len := lens.get! i
        (routes.get! i).get! (t % len)
      let mut know := know
      for i in [0:n] do
        for j in [i+1:n] do
          if pos.get! i = pos.get! j then
            let u := (know.get! i) ||| (know.get! j)
            know := know.set! i u
            know := know.set! j u
      if know.foldl (fun b k => b && k = target) true then
        some t
      else
        loop (t+1) know
  match loop 0 initKnow with
  | some t => toString t
  | none => "NEVER"

partial def readRoutes (toks : Array String) (idx n : Nat) (acc : Array (Array Nat)) : (Array (Array Nat) × Nat) :=
  if n = 0 then
    (acc, idx)
  else
    let s := toks.get! idx |>.toNat!
    let rec readStops (k idx : Nat) (arr : Array Nat) : (Array Nat × Nat) :=
      if k = 0 then (arr, idx) else
        let v := toks.get! idx |>.toNat!
        readStops (k-1) (idx+1) (arr.push v)
    let (arr, idx') := readStops s (idx+1) #[]
    readRoutes toks idx' (n-1) (acc.push arr)

partial def readCases (toks : Array String) (idx : Nat) (acc : List String) : List String :=
  if h : idx < toks.size then
    let n := toks.get! idx |>.toNat!
    if n = 0 then
      acc.reverse
    else
      let (routes, idx') := readRoutes toks (idx+1) n #[]
      let res := simulate routes
      readCases toks idx' (res :: acc)
  else
    acc.reverse

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
               |>.filter (fun s => s ≠ "")
               |> Array.ofList
  let outs := readCases toks 0 []
  for o in outs do
    IO.println o
