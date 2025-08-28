/- Solution for SPOJ ARCTAN - Use of Function Arctan
https://www.spoj.com/problems/ARCTAN/
-/

import Std
open Std

partial def solve (tokens : Array String) (idx t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let A := (tokens.get! idx).toNat!
    let N := A*A + 1
    let sqrtN := Nat.sqrt N
    let mut d := 1
    let mut best := N + 1
    while d ≤ sqrtN do
      if N % d == 0 then
        let s := d + N / d
        if s < best then
          best := s
      d := d + 1
    let ans := best + 2*A
    IO.println ans
    solve tokens (idx+1) (t-1)

def main : IO Unit := do
  let data ← IO.readStdin
  let tokens := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |> Array.ofList
  if tokens.size = 0 then return ()
  let t := (tokens.get! 0).toNat!
  solve tokens 1 t
