/- Solution for SPOJ TRANS - Transformation
https://www.spoj.com/problems/TRANS/
-/

import Std
open Std

/-- Parse all natural numbers from stdin. -/
def readInts : IO (Array Nat) := do
  let data ← IO.getStdin.readToEnd
  let mut arr : Array Nat := #[]
  let mut num : Nat := 0
  let mut inNum := false
  for c in data.data do
    if c ≥ 48 && c ≤ 57 then
      num := num * 10 + (c.toNat - 48)
      inNum := true
    else if inNum then
      arr := arr.push num
      num := 0
      inNum := false
  if inNum then arr := arr.push num
  return arr

/-- Compute minimal expansion steps to generate substring `y[i..j]` (inclusive) from symbol `a`. -/
def buildDP (y : Array Nat)
    (r2 : Array (Array (Nat × Nat)))
    (r3 : Array (Array (Nat × Nat × Nat))) :
    Array (Array (Array Nat)) :=
  Id.run do
    let m := y.size
    let inf := (1000000000 : Nat)
    let mut dp : Array (Array (Array Nat)) :=
      Array.mkArray 31 (Array.mkArray m (Array.mkArray m inf))
    -- base cases for length 1
    for i in [0:m] do
      let a := y.get! i
      let arrA := dp.get! a
      let row := arrA.get! i |>.set! i 0
      let arrA := arrA.set! i row
      dp := dp.set! a arrA
    -- increasing lengths
    for len in [2:m+1] do
      for i in [0:m+1-len] do
        let j := i + len - 1
        for a in [1:31] do
          let mut best := inf
          -- two-element rules
          for rc in r2.get! a do
            let b := rc.1
            let c := rc.2
            for k in [i:j] do
              let s1 := ((dp.get! b).get! i).get! k
              let s2 := ((dp.get! c).get! (k+1)).get! j
              let v := s1 + s2 + 1
              if v < best then best := v
          -- three-element rules
          for rc in r3.get! a do
            let b := rc.1
            let c := rc.2
            let d := rc.3
            for k1 in [i:j] do
              for k2 in [k1+1:j] do
                let s1 := ((dp.get! b).get! i).get! k1
                let s2 := ((dp.get! c).get! (k1+1)).get! k2
                let s3 := ((dp.get! d).get! (k2+1)).get! j
                let v := s1 + s2 + s3 + 1
                if v < best then best := v
          let arrA := dp.get! a
          let row := arrA.get! i |>.set! j best
          let arrA := arrA.set! i row
          dp := dp.set! a arrA
    return dp

/-- Solve a single test case. -/
def solveCase (x y : Array Nat)
    (r2 : Array (Array (Nat × Nat)))
    (r3 : Array (Array (Nat × Nat × Nat))) : Int :=
  let n := x.size
  let m := y.size
  if n > m then
    (-1)
  else
    let inf := (1000000000 : Nat)
    let dp := buildDP y r2 r3
    -- DP over prefixes of Y for each symbol in X
    let mut prev : Array Nat := Array.mkArray (m+1) inf
    prev := prev.set! 0 0
    for a in x do
      let mut next : Array Nat := Array.mkArray (m+1) inf
      for i in [0:m] do
        let base := prev.get! i
        if base < inf then
          let arrA := dp.get! a
          for j in [i:m] do
            let cost := (arrA.get! i).get! j
            if cost < inf then
              let tot := base + cost
              let idx := j + 1
              if tot < next.get! idx then
                next := next.set! idx tot
      prev := next
    let ans := prev.get! m
    if ans ≥ inf then (-1) else Int.ofNat ans

/-- Main solve function. -/
def solve : IO Unit := do
  let nums ← readInts
  let mut idx := 0
  let t := nums.get! idx
  idx := idx + 1
  let mut outLines : Array String := #[]
  for _ in [0:t] do
    let n := nums.get! idx
    let m := nums.get! (idx+1)
    let u := nums.get! (idx+2)
    let v := nums.get! (idx+3)
    idx := idx + 4
    let mut x : Array Nat := Array.mkEmpty n
    for _ in [0:n] do
      x := x.push (nums.get! idx)
      idx := idx + 1
    let mut y : Array Nat := Array.mkEmpty m
    for _ in [0:m] do
      y := y.push (nums.get! idx)
      idx := idx + 1
    let mut r2 : Array (Array (Nat × Nat)) := Array.mkArray 31 #[]
    for _ in [0:u] do
      let a := nums.get! idx
      let b := nums.get! (idx+1)
      let c := nums.get! (idx+2)
      idx := idx + 3
      r2 := r2.set! a ((r2.get! a).push (b,c))
    let mut r3 : Array (Array (Nat × Nat × Nat)) := Array.mkArray 31 #[]
    let tern := v - u
    for _ in [0:tern] do
      let a := nums.get! idx
      let b := nums.get! (idx+1)
      let c := nums.get! (idx+2)
      let d := nums.get! (idx+3)
      idx := idx + 4
      r3 := r3.set! a ((r3.get! a).push (b,c,d))
    let res := solveCase x y r2 r3
    outLines := outLines.push (toString res)
  IO.println (String.intercalate "\n" outLines)

def main : IO Unit := solve
