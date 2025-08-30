/- Solution for SPOJ MEMDIS - Memory Distribution
https://www.spoj.com/problems/MEMDIS/
-/

import Std
open Std

structure Running where
  finish : Nat
  start : Nat
  len   : Nat
  deriving Inhabited

def eraseIdx {α} (arr : Array α) (i : Nat) : Array α :=
  let left := arr.extract 0 i
  let right := arr.extract (i+1) arr.size
  left ++ right

/-- Insert a free block and merge overlaps. -/
def insertFree (free : Array (Nat × Nat)) (start len : Nat) : Array (Nat × Nat) :=
  let arr := (free.push (start, len)).qsort (fun a b => a.fst < b.fst)
  Id.run do
    let mut res : Array (Nat × Nat) := #[]
    for (s, l) in arr do
      if res.isEmpty then
        res := res.push (s, l)
      else
        let last := res.back!
        let ps := last.fst
        let pl := last.snd
        let pe := ps + pl
        let e := s + l
        if s ≤ pe then
          let newLen := Nat.max pe e - ps
          res := res.set! (res.size - 1) (ps, newLen)
        else
          res := res.push (s, l)
    return res

/-- Allocate a block of size `m` if possible. -/
def alloc (free : Array (Nat × Nat)) (m : Nat) : Option (Nat × Array (Nat × Nat)) :=
  let rec go (i : Nat) :=
    if h : i < free.size then
      let s := free[i]!.fst
      let l := free[i]!.snd
      if l ≥ m then
        let addr := s
        let arr :=
          if l = m then eraseIdx free i
          else free.set! i (s + m, l - m)
        some (addr, arr)
      else
        go (i+1)
    else
      none
  go 0

/-- Insert running program sorted by finish time. -/
def runningInsert (run : List Running) (r : Running) : List Running :=
  match run with
  | [] => [r]
  | h :: t =>
      if r.finish < h.finish then r :: h :: t
      else h :: runningInsert t r

/-- Free all programs finished by time `t`. -/
def freeFinished (run : List Running) (t : Nat) (free : Array (Nat × Nat)) :
    (List Running × Array (Nat × Nat)) :=
  match run with
  | [] => ([], free)
  | h :: rest =>
      if h.finish ≤ t then
        let free' := insertFree free h.start h.len
        freeFinished rest t free'
      else
        (run, free)

/-- Try allocating from queue at current time. -/
def tryQueue (cur : Nat) (queue : Array (Nat × Nat)) (qhead : Nat)
  (free : Array (Nat × Nat)) (run : List Running) :
  (Nat × Array (Nat × Nat) × List Running) :=
  Id.run do
    let mut free := free
    let mut run := run
    let mut head := qhead
    while head < queue.size do
      let (m, p) := queue[head]!
      match alloc free m with
      | some (addr, free') =>
          free := free'
          run := runningInsert run {finish := cur + p, start := addr, len := m}
          head := head + 1
      | none =>
          break
    return (head, free, run)

partial def processCase (h : IO.FS.Stream) (n : Nat) : IO (Nat × Nat) := do
  let mut free : Array (Nat × Nat) := #[(0, n)]
  let mut run : List Running := []
  let mut queue : Array (Nat × Nat) := #[]
  let mut qhead : Nat := 0
  let mut queued : Nat := 0
  let mut cur : Nat := 0
  while true do
    let ln := (← h.getLine)
    if ln = "" then
      break
    let parts := ln.trim.split (· = ' ') |>.filter (· ≠ "")
    let x := parts[0]! |>.toNat!
    let m := parts[1]! |>.toNat!
    let p := parts[2]! |>.toNat!
    if x = 0 ∧ m = 0 ∧ p = 0 then
      break
    cur := x
    let (run1, free1) := freeFinished run cur free
    run := run1; free := free1
    let (qh1, free2, run2) := tryQueue cur queue qhead free run
    qhead := qh1; free := free2; run := run2
    match alloc free m with
    | some (addr, free3) =>
        free := free3
        run := runningInsert run {finish := cur + p, start := addr, len := m}
    | none =>
        queue := queue.push (m, p)
        queued := queued + 1
  let (run1, free1) := freeFinished run cur free
  run := run1; free := free1
  let (qh1, free2, run2) := tryQueue cur queue qhead free run
  qhead := qh1; free := free2; run := run2
  while qhead < queue.size ∨ !run.isEmpty do
    if run.isEmpty then
      let (qh2, free3, run3) := tryQueue cur queue qhead free run
      qhead := qh2; free := free3; run := run3
      if run.isEmpty then break
    else
      let next := (run.head!).finish
      cur := next
      let (run3, free3) := freeFinished run cur free
      run := run3; free := free3
      let (qh2, free4, run4) := tryQueue cur queue qhead free run
      qhead := qh2; free := free4; run := run4
  return (cur, queued)

partial def loopCases (h : IO.FS.Stream) : IO Unit := do
  let ln := (← h.getLine)
  if ln = "" then
    pure ()
  else
    let n := ln.trim.toNat!
    let (finish, cnt) ← processCase h n
    IO.println s!"{finish}"
    IO.println s!"{cnt}"
    loopCases h

def main : IO Unit := do
  let h ← IO.getStdin
  loopCases h
