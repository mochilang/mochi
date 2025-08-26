/- Solution for SPOJ SHPATH - The Shortest Path
https://www.spoj.com/problems/SHPATH/
-/

import Std
open Std

structure Edge where
  to : Nat
  cost : Nat

def dijkstra (adj : Array (Array Edge)) (start dest : Nat) : Nat :=
  let n := adj.size - 1
  let inf := (200000000 : Nat)
  let rec loop (dist : Array Nat) (vis : Array Bool) (step : Nat) : Nat :=
    if step == n then
      dist.get! dest
    else
      let (u, best) := Id.run do
        let mut u := 0
        let mut best := inf
        for i in [1:n+1] do
          if !vis.get! i && dist.get! i < best then
            best := dist.get! i
            u := i
        pure (u, best)
      if u == 0 || u == dest then
        dist.get! dest
      else
        let vis := vis.set! u true
        let dist := Id.run do
          let mut d := dist
          for e in adj.get! u do
            let v := e.to
            let w := e.cost
            if !vis.get! v then
              let alt := best + w
              if alt < d.get! v then
                d := d.set! v alt
          pure d
        loop dist vis (step + 1)
  let dist0 := (Array.mkArray (n+1) inf).set! start 0
  let vis0 := Array.mkArray (n+1) false
  loop dist0 vis0 0

def findCity (names : Array String) (s : String) : Nat :=
  Id.run do
    let mut idx := 0
    for i in [1:names.size] do
      if names.get! i = s then
        idx := i
    pure idx

def readGraph (h : IO.FS.Stream) (n : Nat)
    : IO (Array (Array Edge) × Array String) := do
  let mut adj : Array (Array Edge) := Array.mkArray (n+1) #[]
  let mut names : Array String := Array.mkArray (n+1) ""
  for i in [1:n+1] do
    let name := (← h.getLine).trim
    names := names.set! i name
    let p := (← h.getLine).trim.toNat!
    let mut edges : Array Edge := Array.mkEmpty p
    for _ in [0:p] do
      let line := (← h.getLine).trim
      let parts := line.splitOn " "
      let nr := parts.get! 0 |>.toNat!
      let cost := parts.get! 1 |>.toNat!
      edges := edges.push {to := nr, cost := cost}
    adj := adj.set! i edges
  pure (adj, names)

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then
    pure ()
  else
    let n := (← h.getLine).trim.toNat!
    let (adj, names) ← readGraph h n
    let r := (← h.getLine).trim.toNat!
    for _ in [0:r] do
      let line := (← h.getLine).trim
      let parts := line.splitOn " "
      let src := findCity names (parts.get! 0)
      let dst := findCity names (parts.get! 1)
      let res := dijkstra adj src dst
      IO.println res
    -- consume blank line if any
    try
      discard (← h.getLine)
    catch _ => pure ()
    process h (t-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
