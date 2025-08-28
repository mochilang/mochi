/- Solution for SPOJ TRIOPT - Trigonometric optimization
https://www.spoj.com/problems/TRIOPT/
-/

import Std
open Std

-- evaluate either sin or cos on a natural number
def trig (kind : String) (n : Nat) : Float :=
  let x := Float.ofNat n
  if kind = "sin" then Float.sin x else Float.cos x

-- brute force search over all pairs (x,y)
def solveCase (f1 f2 f3 op : String) (S : Nat) : Float :=
  let maximize := op = "max"
  let mut best : Float := if maximize then -1.0e9 else 1.0e9
  for x in [1:(S+1)] do
    for y in [1:(S+1)] do
      if x + y < S then
        let z := S - x - y
        if z > 0 then
          let value := trig f1 x + trig f2 y + trig f3 z
          if maximize then
            if value > best then best := value
          else
            if value < best then best := value
  best

-- format float with up to ten digits after decimal point
def formatFloat (x : Float) : String :=
  let y := (Float.round (x * 10000000000.0)) / 10000000000.0
  let s := y.toString
  if s.endsWith ".0" then s.dropRight 2 else s

def main : IO Unit := do
  let tLine ← IO.getLine
  let t := tLine.trim.toNat!
  for _ in [0:t] do
    let f1 ← IO.getLine
    let f2 ← IO.getLine
    let f3 ← IO.getLine
    let op ← IO.getLine
    let sLine ← IO.getLine
    let s := sLine.trim.toNat!
    let ans := solveCase f1.trim f2.trim f3.trim op.trim s
    IO.println (formatFloat ans)
