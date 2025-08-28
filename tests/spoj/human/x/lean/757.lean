/- Solution for SPOJ TEM - Thermal Luminescence
https://www.spoj.com/problems/TEM/
-/

import Std
open Std

private def parseNats (s : String) : Array Nat :=
  (s.split (· = ' ') |>.filter (· ≠ "") |>.map String.toNat!).toArray

private def parseInts (s : String) : Array Int :=
  (s.split (· = ' ') |>.filter (· ≠ "") |>.map String.toInt!).toArray

partial def solve (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let header := (← h.getLine).trim
    if header.isEmpty then
      solve h t
    else
      let dims := parseNats header
      let x := dims[0]!
      let y := dims[1]!
      let z := dims[2]!
      let mut arr := Array.replicate (x+1) (Array.replicate (y+1) (Array.replicate (z+1) (0:Int)))
      for xi in [1:x+1] do
        for yi in [1:y+1] do
          let nums := parseInts (← h.getLine).trim
          for zi in [1:z+1] do
            let row := (arr[xi]![yi]!).set! zi (nums[zi-1]!)
            let plane := (arr[xi]!).set! yi row
            arr := arr.set! xi plane
      -- build prefix sums
      let mut pref := Array.replicate (x+1) (Array.replicate (y+1) (Array.replicate (z+1) (0:Int)))
      for xi in [1:x+1] do
        for yi in [1:y+1] do
          for zi in [1:z+1] do
            let s := arr[xi]![yi]![zi]! +
                     pref[xi-1]![yi]![zi]! + pref[xi]![yi-1]![zi]! + pref[xi]![yi]![zi-1]! -
                     pref[xi-1]![yi-1]![zi]! - pref[xi-1]![yi]![zi-1]! - pref[xi]![yi-1]![zi-1]! +
                     pref[xi-1]![yi-1]![zi-1]!
            let row := (pref[xi]![yi]!).set! zi s
            let plane := (pref[xi]!).set! yi row
            pref := pref.set! xi plane
      -- search best cuboid
      let mut best : Int := arr[1]![1]![1]!
      let mut bx := 1
      let mut by := 1
      let mut bz := 1
      let mut ex := 1
      let mut ey := 1
      let mut ez := 1
      for x1 in [1:x+1] do
        for x2 in [x1:x+1] do
          for y1 in [1:y+1] do
            for y2 in [y1:y+1] do
              for z1 in [1:z+1] do
                for z2 in [z1:z+1] do
                  let sum := pref[x2]![y2]![z2]! - pref[x1-1]![y2]![z2]! - pref[x2]![y1-1]![z2]! - pref[x2]![y2]![z1-1]! +
                             pref[x1-1]![y1-1]![z2]! + pref[x1-1]![y2]![z1-1]! + pref[x2]![y1-1]![z1-1]! - pref[x1-1]![y1-1]![z1-1]!
                  if sum > best then
                    best := sum
                    bx := x1; by := y1; bz := z1
                    ex := x2; ey := y2; ez := z2
      IO.println s!"{bx} {by} {bz} {ex} {ey} {ez}"
      solve h (t-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  solve h t
