-- Solution for SPOJ TOANDFRO - To and Fro
-- https://www.spoj.com/problems/TOANDFRO
import Std
open Std

def decode (c : Nat) (s : String) : String :=
  let arr := s.data.toArray
  let rows := arr.size / c
  let chars := (List.range c).bind (fun col =>
                (List.range rows).map (fun row =>
                  let idx := if row % 2 = 0 then row * c + col
                             else row * c + (c - 1 - col)
                  arr[idx]!))
  String.mk chars

partial def process (h : IO.FS.Stream) : IO Unit := do
  let line ← h.getLine
  let cols := line.trim.toNat!
  if cols = 0 then
    pure ()
  else
    let enc ← h.getLine
    let dec := decode cols enc.trim
    IO.println dec
    process h

def main : IO Unit := do
  process (← IO.getStdin)
