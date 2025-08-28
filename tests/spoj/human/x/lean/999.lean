/- Solution for SPOJ MATRIOSH - Generalized Matrioshkas
https://www.spoj.com/problems/MATRIOSH/
-/

import Std
open Std

private def parseInts (s : String) : List Int :=
  s.split (· = ' ') |>.filter (· ≠ "") |>.map String.toInt!

private def isMatrioshka (xs : List Int) : Bool :=
  let rec loop (stack : List (Int × Int)) (rest : List Int) : Bool :=
    match rest with
    | [] => stack == []
    | n :: ns =>
        if n < 0 then
          loop ((-n, 0) :: stack) ns
        else
          match stack with
          | [] => false
          | (size, sum) :: stk =>
              if n != size || sum >= size then false
              else
                let stk :=
                  match stk with
                  | [] => stk
                  | (psize, psum) :: tail => (psize, psum + n) :: tail
                loop stk ns
  loop [] xs

partial def process (h : IO.FS.Stream) : IO Unit := do
  let eof ← h.isEof
  if eof then
    pure ()
  else
    let line := (← h.getLine).trim
    if line.isEmpty then
      process h
    else
      let nums := parseInts line
      let res := if isMatrioshka nums then ":-) Matrioshka!" else ":-( Try again."
      IO.println res
      process h

def main : IO Unit := do
  process (← IO.getStdin)
