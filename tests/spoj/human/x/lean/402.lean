/- Solution for SPOJ HIKE - Hike on a Graph
https://www.spoj.com/problems/HIKE/
-/

import Std
open Std

/-- encode triple of positions into single state index --/
def encode (a b c n : Nat) : Nat := (a * n + b) * n + c

/-- decode state index back into triple of positions --/
def decode (s n : Nat) : Nat × Nat × Nat :=
  let c := s % n
  let s := s / n
  let b := s % n
  let a := s / n
  (a, b, c)

/-- breadth-first search for minimal moves --/
partial def minMoves (n : Nat) (p1 p2 p3 : Nat)
    (mat : Array (Array Char)) : Option Nat := Id.run do
  let size := n * n * n
  let start := encode p1 p2 p3 n
  let mut dist : Array Int := Array.mkArray size (-1)
  let mut q : Std.Queue Nat := .empty
  dist := dist.set! start 0
  q := q.enqueue start
  let mut answer : Option Nat := none
  while true do
    match q.dequeue? with
    | none => break
    | some (state, q') =>
        q := q'
        let d := dist.get! state
        let (a,b,c) := decode state n
        if a = b ∧ b = c then
          answer := some (Int.toNat d)
          break
        let colBC := (mat.get! b).get! c
        for x in [0:n] do
          if (mat.get! a).get! x = colBC then
            let ns := encode x b c n
            if dist.get! ns = -1 then
              dist := dist.set! ns (d + 1)
              q := q.enqueue ns
        let colAC := (mat.get! a).get! c
        for y in [0:n] do
          if (mat.get! b).get! y = colAC then
            let ns := encode a y c n
            if dist.get! ns = -1 then
              dist := dist.set! ns (d + 1)
              q := q.enqueue ns
        let colAB := (mat.get! a).get! b
        for z in [0:n] do
          if (mat.get! c).get! z = colAB then
            let ns := encode a b z n
            if dist.get! ns = -1 then
              dist := dist.set! ns (d + 1)
              q := q.enqueue ns
  return answer

/-- process input tokens recursively --/
partial def solve (toks : Array String) (idx : Nat)
    (acc : List String) : List String :=
  let n := toks.get! idx |>.toNat!
  if n = 0 then acc.reverse else
    let p1 := toks.get! (idx+1) |>.toNat! - 1
    let p2 := toks.get! (idx+2) |>.toNat! - 1
    let p3 := toks.get! (idx+3) |>.toNat! - 1
    let mut pos := idx + 4
    let mut mat : Array (Array Char) := Array.mkEmpty n
    for _ in [0:n] do
      let mut row : Array Char := Array.mkEmpty n
      for _ in [0:n] do
        let ch := (toks.get! pos).data.get! 0
        pos := pos + 1
        row := row.push ch
      mat := mat.push row
    let res := match minMoves n p1 p2 p3 mat with
      | some d => toString d
      | none => "impossible"
    solve toks pos (res :: acc)

/-- main entry point --/
def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (· ≠ "")
                |> Array.ofList
  let outs := solve toks 0 []
  IO.println (String.intercalate "\n" outs)
