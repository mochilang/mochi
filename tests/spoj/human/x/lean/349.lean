/- Solution for SPOJ AROUND - Around the world
https://www.spoj.com/problems/AROUND/
-/

import Std
open Std

/-- Read all integers from stdin. --/
def readInts : IO (Array Nat) := do
  let h ← IO.getStdin
  let s ← h.readToEnd
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Nat := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toNat!
  return arr

/-- Minimum flights to circle the earth starting and ending at farm 1. --/
def minFlights (n : Nat) (adj : Array (Array (Nat × Int))) : Int := Id.run do
  let R : Int := 720
  let width : Nat := Int.toNat (2 * R + 1)
  let size := (n + 1) * width
  let mut visited : Array Bool := Array.mkArray size false
  let mut queue : Array (Nat × Int × Nat) := #[]
  let mut head : Nat := 0
  let startIdx := 1 * width + Int.toNat (R)
  visited := visited.set! startIdx true
  queue := queue.push (1, 0, 0)
  let mut ans : Int := -1
  while head < queue.size && ans == -1 do
    let (u, diff, steps) := queue[head]!; head := head + 1
    for (v, w) in adj[u]! do
      let diff2 := diff + w
      if diff2 ≥ -R && diff2 ≤ R then
        let idx := v * width + Int.toNat (diff2 + R)
        if !visited.get! idx then
          if v == 1 && diff2 ≠ 0 && diff2 % 360 == 0 then
            ans := Int.ofNat (steps + 1)
          visited := visited.set! idx true
          queue := queue.push (v, diff2, steps + 1)
  return ans

/-- Solve a single test case starting at `start` in `data`. --/
partial def solveCase (data : Array Nat) (start : Nat) : (String × Nat) := Id.run do
  let n := data[start]!
  let m := data[start + 1]!
  let mut idx := start + 2
  let mut lon : Array Int := Array.mkArray (n + 1) 0
  for i in [1:n+1] do
    lon := lon.set! i (Int.ofNat (data[idx]!))
    idx := idx + 1
  let mut adj : Array (Array (Nat × Int)) := Array.replicate (n + 1) #[]
  for _ in [0:m] do
    let a := data[idx]!; idx := idx + 1
    let b := data[idx]!; idx := idx + 1
    let l1 := lon[a]!
    let l2 := lon[b]!
    let cw := Int.emod (l2 - l1 + 360) 360
    let w := if cw ≤ 180 then cw else cw - 360
    adj := adj.set! a ((adj.get! a).push (b, w))
    adj := adj.set! b ((adj.get! b).push (a, -w))
  let ans := minFlights n adj
  (toString ans, idx)

/-- Main program: parse input and solve each test case. --/
def main : IO Unit := do
  let data ← readInts
  let t := data.get! 0
  let mut idx := 1
  for _ in [0:t] do
    let (out, idx') := solveCase data idx
    IO.println out
    idx := idx'
