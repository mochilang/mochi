/- Solution for SPOJ PICAD - Crime at Picadily Circus
https://www.spoj.com/problems/PICAD/
-/

import Std
open Std

private def parsePair (s : String) : Nat × Nat :=
  let parts := s.splitOn " "
  (parts.get! 0 |>.toNat!, parts.get! 1 |>.toNat!)

partial def loop (h : IO.FS.Stream) : IO Unit := do
  match (← h.getLine?).map String.trim with
  | none => pure ()
  | some line =>
    if line.isEmpty then
      loop h
    else
      let (p, k) := parsePair line
      let n := (← h.getLine).trim.toNat!
      let mut events : Std.HashMap Nat Int := {}
      events := events.insert p 0
      events := events.insert (k + 1) 0
      for _ in [0:n] do
        let ln := (← h.getLine).trim
        if ln ≠ "" then
          let (a, b) := parsePair ln
          if ¬(b < p || a > k) then
            let s := max a p
            let e := min b k
            let v1 := events.findD s 0
            events := events.insert s (v1 + 1)
            let e1 := e + 1
            let v2 := events.findD e1 0
            events := events.insert e1 (v2 - 1)
      let mut arr := events.toList.toArray
      arr := arr.qsort (fun x y => x.fst < y.fst)
      let mut cnt : Int := 0
      let mut mn : Int := Int.ofNat n
      let mut mx : Int := 0
      for i in [0:arr.size] do
        let (t, d) := arr.get! i
        cnt := cnt + d
        if t ≤ k then
          mn := Int.min mn cnt
          mx := Int.max mx cnt
      IO.println s!"{mn.toNat} {mx.toNat}"
      loop h

def main : IO Unit :=
  loop (← IO.getStdin)
