/- Solution for SPOJ CHICAGO - 106 miles to Chicago
https://www.spoj.com/problems/CHICAGO/
-/

import Std
open Std

-- insert into priority queue sorted by probability (descending)
private def insertPQ (p : Float × Nat) (pq : List (Float × Nat)) : List (Float × Nat) :=
  match pq with
  | [] => [p]
  | q :: qs => if p.fst > q.fst then p :: pq else q :: insertPQ p qs

-- compute maximum success probability from node 1 to node n
private def maxProb (n : Nat) (adj : Array (List (Nat × Float))) : Float :=
  Id.run <| do
    let mut best := Array.mkArray (n+1) 0.0
    let mut pq : List (Float × Nat) := []
    best := best.set! 1 1.0
    pq := insertPQ (1.0, 1) pq
    let rec loop (best : Array Float) (pq : List (Float × Nat)) : Float :=
      match pq with
      | [] => best.get! n
      | (prob, v) :: qs =>
        if v = n then prob
        else if prob < best.get! v then
          loop best qs
        else
          let mut best := best
          let mut pq := qs
          for (to, w) in adj.get! v do
            let np := prob * w
            if np > best.get! to then
              best := best.set! to np
              pq := insertPQ (np, to) pq
          loop best pq
    loop best pq

-- format float with six decimal places
private def format6 (x : Float) : String :=
  let y := x + 0.0000005
  let s := y.toString
  let parts := s.splitOn "."
  let intPart := parts.get! 0
  let fracPart :=
    if h : parts.length > 1 then
      let f := parts.get! 1
      (f ++ "000000").take 6
    else
      "000000"
  intPart ++ "." ++ fracPart

partial def solveTokens (toks : Array String) (idx : Nat) (acc : List String) : List String :=
  let n := toks.get! idx |>.toNat!
  if n = 0 then acc.reverse else
    let m := toks.get! (idx+1) |>.toNat!
    let mut adj : Array (List (Nat × Float)) := Array.mkArray (n+1) []
    let mut i := idx + 2
    for _ in [0:m] do
      let a := toks.get! i |>.toNat!
      let b := toks.get! (i+1) |>.toNat!
      let p := toks.get! (i+2) |>.toNat!
      let prob := (Float.ofNat p) / 100.0
      adj := adj.set! a ((b, prob) :: adj.get! a)
      adj := adj.set! b ((a, prob) :: adj.get! b)
      i := i + 3
    let ans := maxProb n adj
    let line := format6 (ans * 100.0) ++ " percent"
    solveTokens toks i (line :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
             |>.filter (fun s => s ≠ "")
             |> Array.ofList
  let lines := solveTokens toks 0 []
  for line in lines do
    IO.println line
