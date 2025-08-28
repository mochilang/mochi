/- Solution for SPOJ SAM - Toy Cars
https://www.spoj.com/problems/SAM/
-/

import Std
open Std

-- Solve one test case
def solveCase (n k p : Nat) (seq : Array Nat) : Nat :=
  Id.run do
    -- build future occurrence lists for each car
    let mut occurs : Array (List Nat) := Array.replicate (n+1) []
    for off in [0:p] do
      let j := p - 1 - off
      let c := seq[j]!
      occurs := occurs.set! c (j :: occurs[c]!)
    -- simulate using Belady's optimal replacement
    let mut floor : HashSet Nat := {}
    let mut nextMap : HashMap Nat Nat := {}
    let inf := p + 1
    let mut ans := 0
    for idx in [0:p] do
      let c := seq[idx]!
      let occList := occurs[c]!
      let rest := match occList with
        | _ :: xs => xs
        | [] => []
      occurs := occurs.set! c rest
      let nextIdx := match rest with
        | r :: _ => r
        | [] => inf
      if floor.contains c then
        nextMap := nextMap.insert c nextIdx
      else
        ans := ans + 1
        if floor.size == k then
          let mut farCar := 0
          let mut farIdx := 0
          for (car, ni) in nextMap.toList do
            if ni > farIdx then
              farIdx := ni
              farCar := car
          floor := floor.erase farCar
          nextMap := nextMap.erase farCar
        floor := floor.insert c
        nextMap := nextMap.insert c nextIdx
    return ans

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  let rec loop : Nat → IO Unit
    | 0 => pure ()
    | Nat.succ c => do
        let line ← h.getLine
        let parts := line.trim.split (· = ' ')
        let arr := parts.toArray
        let n := arr[0]!.toNat!
        let k := arr[1]!.toNat!
        let p := arr[2]!.toNat!
        let mut seq : Array Nat := #[]
        for _ in [0:p] do
          let s ← h.getLine
          seq := seq.push (s.trim.toNat!)
        let ans := solveCase n k p seq
        IO.println ans
        loop c
  loop t
