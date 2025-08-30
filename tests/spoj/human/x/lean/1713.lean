/- Solution for SPOJ SCALE - Funny scales
https://www.spoj.com/problems/SCALE/
-/

import Std
open Std

partial def balance (n : Nat) (x0 : Int) : Option (List Nat × List Nat) :=
  let sum : Int := ((Int.ofNat 3) ^ n - 1) / 2
  if x0 > sum then none
  else
    let rec loop (i : Nat) (x : Int) (left right : List Nat) : Option (List Nat × List Nat) :=
      if x == 0 then some (left.reverse, right.reverse)
      else if h : i < n then
        let r := x % 3
        let q := x / 3
        if r == 0 then
          loop (i+1) q left right
        else if r == 1 then
          loop (i+1) q left ((i+1)::right)
        else
          loop (i+1) (q+1) ((i+1)::left) right
      else none
    loop 0 x0 [] []

def main : IO Unit := do
  let h ← IO.getStdin
  let line ← h.getLine
  let ws := line.trim.splitOn " "
  let n := (ws.getD 0 "0").toNat!
  let x := (ws.getD 1 "0").toInt!
  match balance n x with
  | none => IO.println "-1"
  | some (l, r) =>
      IO.println (String.intercalate " " (l.map toString))
      IO.println (String.intercalate " " (r.map toString))
