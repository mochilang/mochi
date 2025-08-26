/- Solution for SPOJ DYZIO - Dyzio
https://www.spoj.com/problems/DYZIO/
-/

import Std
open Std

partial def solveCase (s : String) : Nat :=
  let arr := s.data.toArray
  let n := arr.size
  let rec loop (i depth parent cut maxd ans : Nat) (stack : List (Nat × Nat)) : Nat :=
    if i < n then
      let c := arr.get! i
      if c = '0' then
        let (maxd, ans) := if depth > maxd then (depth, parent) else (maxd, ans)
        match stack with
        | [] => ans
        | (d, p) :: st => loop (i+1) d p cut maxd ans st
      else
        let cut := cut + 1
        let parent := cut
        loop (i+1) (depth+1) parent cut maxd ans ((depth+1, parent) :: stack)
    else
      ans
  loop 0 0 0 0 0 0 []

partial def process (h : IO.FS.Stream) (n : Nat) : IO Unit := do
  if n = 0 then
    pure ()
  else
    let _ ← h.getLine
    let s ← h.getLine
    IO.println (solveCase s.trim)
    process h (n-1)

def main : IO Unit := do
  process (← IO.getStdin) 10
