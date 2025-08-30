/- Solution for SPOJ TRICOUNT - Counting Triangles
https://www.spoj.com/problems/TRICOUNT/
-/

import Std
open Std

def tricount (n : Int) : Int :=
  let res := n * (n + 2) * (2 * n + 1)
  if n % 2 == 0 then
    res / 8
  else
    (res - 1) / 8

def main : IO Unit := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let nums := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |> List.map (fun s => s.toInt!)
  match nums with
  | [] => pure ()
  | _ :: rest =>
      let rec loop : List Int → IO Unit
        | [] => pure ()
        | n :: xs =>
            IO.println (tricount n)
            loop xs
      loop rest
