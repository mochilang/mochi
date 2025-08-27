/- Solution for SPOJ TTABLE - Timetable
https://www.spoj.com/problems/TTABLE/
-/

import Std
open Std

structure Train where
  dep : Nat
  arr : Nat
  to  : Nat

def parseTime (s : String) : Nat :=
  let hh := s.take 2 |>.toNat!
  let mm := s.drop 3 |>.toNat!
  hh * 60 + mm

def two (n : Nat) : String :=
  if n < 10 then "0" ++ toString n else toString n

def formatTime (t : Nat) : String :=
  two (t / 60) ++ ":" ++ two (t % 60)

structure State where
  city : Nat
  time : Nat
  start : Nat

def initQueue (edges : Array Train) : List State :=
  Id.run do
    let mut q : List State := []
    for e in edges do
      q := {city := e.to, time := e.arr, start := e.dep} :: q
    return q

partial def explore (adj : Array (Array Train)) (n : Nat)
    (queue : List State)
    (best : Std.HashMap (Nat × Nat) Nat)
    (res : Std.HashMap Nat Nat) : Std.HashMap Nat Nat :=
  match queue with
  | [] => res
  | st :: rest =>
      let key := (st.city, st.start)
      let (cont, best) :=
        match best.get? key with
        | some v =>
            if st.time <= v then (true, best.insert key st.time) else (false, best)
        | none   => (true, best.insert key st.time)
      if !cont then
        explore adj n rest best res
      else if st.city == n then
        let res :=
          match res.get? st.start with
          | some v => if st.time < v then res.insert st.start st.time else res
          | none   => res.insert st.start st.time
        explore adj n rest best res
      else
        let edges := adj[st.city]!
        let (rest, best) :=
          edges.foldl
            (fun (p : List State × Std.HashMap (Nat × Nat) Nat) e =>
              let (q,b) := p
              if e.dep >= st.time then
                let key2 := (e.to, st.start)
                let (ins, b) :=
                  match b.get? key2 with
                  | some v => if e.arr < v then (true, b.insert key2 e.arr) else (false, b)
                  | none   => (true, b.insert key2 e.arr)
                let q := if ins then {city := e.to, time := e.arr, start := st.start} :: q else q
                (q, b)
              else
                (q, b)) (rest, best)
        explore adj n rest best res

def filterOptimal (pairs : List (Nat × Nat)) : List (Nat × Nat) :=
  let sorted := (pairs.toArray.qsort (fun a b => a.fst < b.fst)).toList
  let rec go (xs : List (Nat × Nat)) : List (Nat × Nat) :=
    match xs with
    | [] => []
    | p :: rest =>
        let dominated := rest.any (fun q => q.fst >= p.fst && q.snd <= p.snd && (q.snd - q.fst) < (p.snd - p.fst))
        let tail := go rest
        if dominated then tail else p :: tail
  go sorted

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let n := (← h.getLine).trim.toNat!
    let mut adj : Array (Array Train) := Array.replicate (n + 1) #[]
    for i in [1:n+1] do
      let m := (← h.getLine).trim.toNat!
      let mut edges : Array Train := #[]
      for _ in [0:m] do
        let ln := (← h.getLine).trim
        let ps := ln.splitOn " "
        let dep := parseTime (ps[0]!)
        let arr := parseTime (ps[1]!)
        let dst := ps[2]! |>.toNat!
        edges := edges.push {dep := dep, arr := arr, to := dst}
      adj := adj.set! i edges
    let init := initQueue (adj[1]!)
    let res := explore adj n init ({} : Std.HashMap (Nat × Nat) Nat) ({} : Std.HashMap Nat Nat)
    let pairs := res.toList
    let opt := filterOptimal pairs
    IO.println opt.length
    for (a,b) in opt do
      IO.println (formatTime a ++ " " ++ formatTime b)
    process h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
