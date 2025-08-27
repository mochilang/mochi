/- Solution for SPOJ ALICEBOB - Alice and Bob
https://www.spoj.com/problems/ALICEBOB/
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

/-- Reconstruct polygon vertex order from edges. -/
partial def reconstruct (n : Nat) (edges : Array (Nat × Nat)) : Array Nat := Id.run do
  let mut adj : Array (Std.HashSet Nat) := Array.mkArray (n+1) {}
  for (a,b) in edges do
    adj := adj.modify a (·.insert b)
    adj := adj.modify b (·.insert a)
  let mut q : Std.Queue Nat := {}
  for i in [1:n+1] do
    if (adj.get! i).size ≤ 2 then
      q := q.enqueue i
  let mut removed : Array (Nat × Nat × Nat) := #[]
  let mut alive : Array Bool := Array.mkArray (n+1) true
  let mut aliveCount := n
  while h : aliveCount > 2 do
    match q.dequeue? with
    | none => break
    | some (v, q') =>
      q := q'
      if alive.get! v && (adj.get! v).size ≤ 2 then
        let ns := (adj.get! v).toList
        let a := ns.get! 0; let b := ns.get! 1
        removed := removed.push (v,a,b)
        alive := alive.set! v false
        aliveCount := aliveCount - 1
        adj := adj.modify a (·.erase v)
        adj := adj.modify b (·.erase v)
        if !(adj.get! a).contains b then
          adj := adj.modify a (·.insert b)
          adj := adj.modify b (·.insert a)
        if (adj.get! a).size ≤ 2 && alive.get! a then
          q := q.enqueue a
        if (adj.get! b).size ≤ 2 && alive.get! b then
          q := q.enqueue b
  let mut rem : List Nat := []
  for i in [1:n+1] do
    if alive.get! i then rem := i :: rem
  let u := rem.get! 0
  let v := rem.get! 1
  let mut nxt : Array Nat := Array.mkArray (n+1) 0
  let mut prv : Array Nat := Array.mkArray (n+1) 0
  nxt := nxt.set! u v; prv := prv.set! v u
  nxt := nxt.set! v u; prv := prv.set! u v
  for t in removed.toList.reverse do
    let x := t.1; let a := t.2; let b := t.3
    if nxt.get! a = b then
      nxt := nxt.set! a x
      prv := prv.set! x a
      nxt := nxt.set! x b
      prv := prv.set! b x
    else
      nxt := nxt.set! b x
      prv := prv.set! x b
      nxt := nxt.set! x a
      prv := prv.set! a x
  let p1 := prv.get! 1
  let n1 := nxt.get! 1
  let forward := if p1 < n1 then false else true
  let mut res : Array Nat := #[1]
  let mut cur := if forward then n1 else p1
  for _ in [0:n-1] do
    res := res.push cur
    cur := if forward then nxt.get! cur else prv.get! cur
  return res

/-- Solve all datasets. -/
def solve : IO Unit := do
  let nums ← readInts
  let mut idx := 0
  let d := nums.get! idx
  idx := idx + 1
  let mut outLines : Array String := #[]
  for _ in [0:d] do
    let n := nums.get! idx; let m := nums.get! (idx+1)
    idx := idx + 2
    let total := n + m
    let mut edges : Array (Nat × Nat) := Array.mkEmpty total
    for _ in [0:total] do
      let a := nums.get! idx; let b := nums.get! (idx+1)
      edges := edges.push (a,b)
      idx := idx + 2
    let order := reconstruct n edges
    let line := String.intercalate " " (order.toList.map (fun x => toString x))
    outLines := outLines.push line
  IO.println (String.intercalate "\n" outLines)

def main : IO Unit := solve
