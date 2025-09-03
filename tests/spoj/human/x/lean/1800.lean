/- Solution for SPOJ CONTEST - Fixed Partition Contest Management
https://www.spoj.com/problems/CONTEST/
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

/-- format average time as string with two decimals --/
def avgStr (total : Nat) (n : Nat) : String :=
  let num := total * 100
  let q := num / n
  let r := num % n
  let q := if r * 2 >= n then q + 1 else q
  let intPart := q / 100
  let frac := q % 100
  let fracStr := if frac < 10 then s!"0{frac}" else toString frac
  s!"{intPart}.{fracStr}"

/-- compute best assignment and schedule --/
partial def solve (m n : Nat) (times : Array (Array (Option Nat))) : (Nat × Array Nat × Array Nat × Array Nat) :=
  -- search all assignments
  let rec search (i : Nat) (assign : Array Nat) : Nat × Array Nat :=
    if h : i = n then
      (eval assign, assign)
    else
      let row := times.get! i
      let mut bestCost := (Nat.succ (Nat.pow 10 9)) -- big number
      let mut bestAssign := assign
      for j in [0:m] do
        match row.get! j with
        | some _ =>
            let (c, a) := search (i+1) (assign.push j)
            if c < bestCost then
              bestCost := c
              bestAssign := a
        | none => pure ()
      (bestCost, bestAssign)
  -- evaluate cost of assignment
  and eval (assign : Array Nat) : Nat := Id.run do
    let mut lists : Array (Array Nat) := Array.mkArray m #[]
    for i in [0:n] do
      let j := assign.get! i
      let p := (times.get! i).get! j |>.get!
      let arr := lists.get! j
      lists := lists.set! j (arr.push p)
    let mut total := 0
    for j in [0:m] do
      let arr := (lists.get! j).qsort (fun a b => a < b)
      let mut t := 0
      for p in arr do
        t := t + p
        total := total + t
    return total
  let (bestCost, bestAssign) := search 0 #[]
  -- build schedule
  let mut start : Array Nat := Array.mkArray n 0
  let mut finish : Array Nat := Array.mkArray n 0
  let mut member : Array Nat := Array.mkArray n 0
  for j in [0:m] do
    let mut arr : Array (Nat × Nat) := #[] -- (idx,time)
    for i in [0:n] do
      if bestAssign.get! i = j then
        let p := (times.get! i).get! j |>.get!
        arr := arr.push (i, p)
    arr := arr.qsort (fun a b => if a.snd == b.snd then a.fst < b.fst else a.snd < b.snd)
    let mut t := 0
    for item in arr do
      let idx := item.fst
      let p := item.snd
      start := start.set! idx t
      let t' := t + p
      finish := finish.set! idx t'
      member := member.set! idx j
      t := t'
  (bestCost, start, finish, member)

/-- Main program --/
def main : IO Unit := do
  let data ← readInts
  let mut idx := 0
  let mut case := 1
  let mut outputs : Array String := #[]
  while idx < data.size do
    let m := data.get! idx; idx := idx + 1
    let n := data.get! idx; idx := idx + 1
    if m == 0 && n == 0 then
      break
    -- read members
    let mut mem : Array Nat := #[]
    for _ in [0:m] do
      mem := mem.push (data.get! idx)
      idx := idx + 1
    -- read problems
    let mut times : Array (Array (Option Nat)) := Array.mkArray n #[]
    for _ in [0:n] do
      let k := data.get! idx; idx := idx + 1
      let mut pairs : Array (Nat × Nat) := #[]
      for _ in [0:k] do
        let s := data.get! idx; let t := data.get! (idx+1)
        idx := idx + 2
        pairs := pairs.push (s, t)
      -- compute times for each member
      let mut row : Array (Option Nat) := Array.mkArray m none
      for j in [0:m] do
        let b := mem.get! j
        let mut t? : Option Nat := none
        for p in pairs do
          if b ≥ p.fst then
            t? := some p.snd
          else
            break
        row := row.set! j t?
      times := times.push row
    -- solve case
    let (total, start, finish, member) := solve m n times
    outputs := outputs.push s!"Case {case}"
    outputs := outputs.push s!"Average solution time = {avgStr total n}"
    for i in [0:n] do
      outputs := outputs.push s!"Problem {i+1} is solved by member {member.get! i + 1} from {start.get! i} to {finish.get! i}"
    outputs := outputs.push ""
    case := case + 1
  -- print all
  for line in outputs do
    IO.println line
