/- Solution for SPOJ MATSUM - Matrix Summation
https://www.spoj.com/problems/MATSUM/
-/

import Std
open Std

partial def handleCase (h : IO.FS.Stream) (n : Nat) : IO Unit := do
  let mut mat : Array (Array Int) := Array.mkArray n (Array.mkArray n (0 : Int))
  let rec loop : IO Unit := do
    let line := (← h.getLine).trim
    let parts := line.splitOn " "
    match parts with
    | ["SET", sx, sy, sval] =>
        let x := sx.toNat!
        let y := sy.toNat!
        let v := sval.toInt!
        let row := (mat.get! x).set! y v
        mat := mat.set! x row
        loop
    | ["SUM", sx1, sy1, sx2, sy2] =>
        let x1 := sx1.toNat!
        let y1 := sy1.toNat!
        let x2 := sx2.toNat!
        let y2 := sy2.toNat!
        let mut s : Int := 0
        for i in [x1:x2+1] do
          let row := mat.get! i
          for j in [y1:y2+1] do
            s := s + row.get! j
        IO.println s
        loop
    | ["END"] =>
        IO.println ""
    | _ =>
        loop
  loop

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  for _ in [0:t] do
    let n := (← h.getLine).trim.toNat!
    handleCase h n
