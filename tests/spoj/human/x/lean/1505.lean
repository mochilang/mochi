/- Solution for SPOJ MOLE - Whac-a-Mole
https://www.spoj.com/problems/MOLE/
-/

import Std
open Std

structure Mole where
  x : Nat
  y : Nat

@[inline] def onSegment (x1 y1 x2 y2 : Nat) (m : Mole) : Bool :=
  let dx1 := (Int.ofNat x2) - (Int.ofNat x1)
  let dy1 := (Int.ofNat y2) - (Int.ofNat y1)
  let dx2 := (Int.ofNat m.x) - (Int.ofNat x1)
  let dy2 := (Int.ofNat m.y) - (Int.ofNat y1)
  dx1 * dy2 == dy1 * dx2 &&
    m.x ≥ Nat.min x1 x2 && m.x ≤ Nat.max x1 x2 &&
    m.y ≥ Nat.min y1 y2 && m.y ≤ Nat.max y1 y2

@[inline] def countOnSegment (moles : List Mole) (x1 y1 x2 y2 : Nat) : Nat :=
  moles.foldl (fun acc m => if onSegment x1 y1 x2 y2 m then acc + 1 else acc) 0

def solveCase (n d : Nat) (groups : Array (List Mole)) (maxT : Nat) : Nat :=
  Id.run do
    let mut prev := Array.replicate n (Array.replicate n 0)
    let d2 : Int := (Int.ofNat d) * (Int.ofNat d)
    for t in [1:maxT+1] do
      let moles := groups[t]!
      let mut curr := Array.replicate n (Array.replicate n 0)
      for x1 in [0:n] do
        for y1 in [0:n] do
          let prevVal := prev[x1]![y1]!
          let xlo := if x1 <= d then 0 else x1 - d
          let xhi := Nat.min (n - 1) (x1 + d)
          for x2 in [xlo:xhi+1] do
            let ylo := if y1 <= d then 0 else y1 - d
            let yhi := Nat.min (n - 1) (y1 + d)
            for y2 in [ylo:yhi+1] do
              let dx := (Int.ofNat x2) - (Int.ofNat x1)
              let dy := (Int.ofNat y2) - (Int.ofNat y1)
              if dx*dx + dy*dy <= d2 then
                let cnt := countOnSegment moles x1 y1 x2 y2
                let val := prevVal + cnt
                let old := curr[x2]![y2]!
                if val > old then
                  let row := curr[x2]!
                  curr := curr.set! x2 (row.set! y2 val)
      prev := curr
    let mut ans := 0
    for x in [0:n] do
      for y in [0:n] do
        let v := prev[x]![y]!
        if v > ans then ans := v
    return ans

partial def process (h : IO.FS.Stream) : IO Unit := do
  let line := (<- h.getLine).trim
  if line.isEmpty then
    return ()
  let parts := line.split (· = ' ') |>.filter (· ≠ "")
  let n := parts[0]! |>.toNat!
  let d := parts[1]! |>.toNat!
  let m := parts[2]! |>.toNat!
  if n == 0 && d == 0 && m == 0 then
    return ()
  let mut groups : Array (List Mole) := Array.replicate 11 []
  let mut maxT : Nat := 0
  for _ in [0:m] do
    let l := (<- h.getLine).trim
    let ps := l.split (· = ' ') |>.filter (· ≠ "")
    let x := ps[0]! |>.toNat!
    let y := ps[1]! |>.toNat!
    let t := ps[2]! |>.toNat!
    maxT := Nat.max maxT t
    let arr := groups[t]!
    groups := groups.set! t ({x, y} :: arr)
  IO.println (solveCase n d groups maxT)
  process h

def main : IO Unit := do
  process (<- IO.getStdin)
