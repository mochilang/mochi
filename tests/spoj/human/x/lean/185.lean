/- Solution for SPOJ CHASE1 - Chase
https://www.spoj.com/problems/CHASE1/
-/

import Std
open Std

structure State where
  a : Nat
  b : Nat
  turn : Bool
  deriving BEq, Hashable

structure Info where
  res : UInt8 -- 0=unknown,1=B win,2=A win
  outdeg : Nat
  dist : Nat
  deriving Inhabited

/-- predecessors of a state --/
def preds (adj : Array (List Nat)) (s : State) : List State :=
  if s.turn then
    -- B to move in s, previous was A's move
    let mut ps : List State := [{a := s.a, b := s.b, turn := false}]
    for na in adj[s.a]! do
      ps := {a := na, b := s.b, turn := false} :: ps
    ps
  else
    -- A to move in s, previous was B's move
    let mut ps : List State := [{a := s.a, b := s.b, turn := true}]
    for nb in adj[s.b]! do
      ps := {a := s.a, b := nb, turn := true} :: ps
    ps

/-- Solve the chase game. Returns number of turns or none if B cannot win. --/
partial def chase (adj : Array (List Nat)) (a0 b0 : Nat) : Option Nat := Id.run do
  let n := adj.size
  let mut info : HashMap State Info := {}
  let mut q : Std.Queue State := .empty
  -- initialize terminal states where players meet
  for v in [0:n] do
    for t in [false, true] do
      let s := {a := v, b := v, turn := t}
      info := info.insert s {res := 1, outdeg := 0, dist := 0}
      q := q.enqueue s
  while true do
    match q.dequeue? with
    | none => break
    | some (s, q') =>
        q := q'
        let some is := info.find? s | continue
        let ps := preds adj s
        for p in ps do
          let defaultOut := if p.turn then adj[p.b]!.length + 1 else adj[p.a]!.length + 1
          let ip := info.findD p {res := 0, outdeg := defaultOut, dist := 0}
          if ip.res != 0 then
            continue
          if (p.turn && is.res == 1) || (!p.turn && is.res == 2) then
            info := info.insert p {res := is.res, outdeg := ip.outdeg, dist := is.dist + 1}
            q := q.enqueue p
          else
            let od := ip.outdeg - 1
            if od == 0 then
              let newRes : UInt8 := if p.turn then 2 else 1
              info := info.insert p {res := newRes, outdeg := od, dist := is.dist + 1}
              q := q.enqueue p
            else
              info := info.insert p {res := 0, outdeg := od, dist := 0}
  let start := {a := a0, b := b0, turn := false}
  match info.find? start with
  | some inf =>
      if inf.res == 1 then
        some ((inf.dist + 1) / 2)
      else
        none
  | none => none

/-- Read all integers from stdin. --/
def readInts : IO (Array Int) := do
  let s ← IO.readToEnd (← IO.getStdin)
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Int := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toInt!
  return arr

/-- Solve a single test case starting at index `start`. --/
partial def solveCase (data : Array Int) (start : Nat) : (String × Nat) := Id.run do
  let n := data.get! start |>.toNat!
  let m := data.get! (start+1) |>.toNat!
  let a0 := data.get! (start+2) |>.toNat!
  let b0 := data.get! (start+3) |>.toNat!
  let mut idx := start + 4
  let mut adj : Array (List Nat) := Array.mkArray n []
  for _ in [0:m] do
    let u := data.get! idx |>.toNat!; idx := idx + 1
    let v := data.get! idx |>.toNat!; idx := idx + 1
    adj := adj.modify (u-1) (fun l => (v-1) :: l)
    adj := adj.modify (v-1) (fun l => (u-1) :: l)
  match chase adj (a0-1) (b0-1) with
  | some t => (toString t, idx)
  | none   => ("No", idx)

/-- Main program: parse input and solve all cases. --/
def main : IO Unit := do
  let data ← readInts
  let t := data.get! 0 |>.toNat!
  let mut idx := 1
  for _ in [0:t] do
    let (res, idx') := solveCase data idx
    IO.println res
    idx := idx'
