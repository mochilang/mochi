/- Solution for SPOJ FTOUR - Free Tour
https://www.spoj.com/problems/FTOUR/
-/
import Std
open Std

/-- Parse all non-negative integers from stdin. -/
def readInts : IO (Array Nat) := do
  let data ← IO.getStdin.readToEnd
  let mut arr : Array Nat := #[]
  let mut num : Nat := 0
  let mut inNum := false
  for c in data.data do
    if c ≥ '0'.toNat && c ≤ '9'.toNat then
      num := num * 10 + (c - '0'.toNat)
      inNum := true
    else if inNum then
      arr := arr.push num
      num := 0
      inNum := false
  if inNum then arr := arr.push num
  return arr

/-- Path from a node up to the root using parent pointers. -/
def pathUp (parent : Array Nat) (u : Nat) : List Nat :=
  Id.run do
    let mut res : List Nat := []
    let mut x := u
    while x ≠ 0 do
      res := x :: res
      x := parent.get! x
    return res.reverse

/-- Take list prefix up to and including the first occurrence of `target`. -/
partial def takeUntil (xs : List Nat) (target : Nat) : List Nat :=
  match xs with
  | [] => []
  | h :: t => if h == target then [h] else h :: takeUntil t target

/-- Reconstruct cycle passing through `v` and `w`. -/
def buildCycle (parent : Array Nat) (v w : Nat) : List Nat :=
  let ru := pathUp parent v
  let rv := pathUp parent w
  let setV : Std.HashSet Nat := ru.foldl (fun s x => s.insert x) {}
  let lca := (rv.find? (fun x => setV.contains x)).getD 0
  let pathV := takeUntil ru lca
  let pathW := takeUntil rv lca
  let pathWRev := (pathW.reverse.drop 1)
  pathV ++ pathWRev ++ [v]

/-- Solve a single test case. -/
def solveCase (n m : Nat) (edges : Array (Nat × Nat)) :
  Option (Nat × List Nat) :=
  Id.run do
    let mut adj : Array (Array Nat) := Array.mkArray (n+1) #[]
    for (a,b) in edges do
      adj := adj.modify a (·.push b)
      adj := adj.modify b (·.push a)
    let INF : Int := 1000000000
    let mut bestLen : Int := INF
    let mut bestCycle : List Nat := []
    for s in [1:n+1] do
      let mut dist : Array Int := Array.mkArray (n+1) (-1)
      let mut parent : Array Nat := Array.mkArray (n+1) 0
      let mut q : Std.Queue Nat := .empty
      dist := dist.set! s 0
      q := q.enqueue s
      while true do
        match q.dequeue? with
        | none => break
        | some (v, q') =>
          q := q'
          for w in adj.get! v do
            if dist.get! w == -1 then
              dist := dist.set! w (dist.get! v + 1)
              parent := parent.set! w v
              q := q.enqueue w
            else if parent.get! v ≠ w then
              let len := dist.get! v + dist.get! w + 1
              if len % 2 == 0 && len < bestLen then
                bestLen := len
                bestCycle := buildCycle parent v w
    if bestLen == INF then
      return none
    else
      return some (Int.toNat bestLen, bestCycle)

/-- Process all datasets. -/
def solve : IO Unit := do
  let nums ← readInts
  let mut idx := 0
  let t := nums.get! idx; idx := idx + 1
  let mut out : Array String := #[]
  for _ in [0:t] do
    let n := nums.get! idx; let m := nums.get! (idx+1)
    idx := idx + 2
    let mut edges : Array (Nat × Nat) := Array.mkEmpty m
    for _ in [0:m] do
      let a := nums.get! idx; let b := nums.get! (idx+1)
      edges := edges.push (a,b)
      idx := idx + 2
    match solveCase n m edges with
    | none =>
        out := out.push "-1"
    | some (len, path) =>
        out := out.push (toString len)
        let line := String.intercalate " " (path.map (fun x => toString x))
        out := out.push line
  IO.println (String.intercalate "\n" out)

def main : IO Unit := solve
