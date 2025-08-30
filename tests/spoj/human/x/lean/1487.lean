/- Solution for SPOJ PT07J - Query on a tree III
https://www.spoj.com/problems/PT07J/
-/
import Std
open Std

structure Node where
  l : Nat
  r : Nat
  cnt : Nat

/-- persistent segment tree update --/
partial def update (nodesRef : IO.Ref (Array Node)) (idx l r pos : Nat) : IO Nat := do
  let nodes <- nodesRef.get
  let n := nodes[idx]!
  if l = r then
    let arr <- nodesRef.get
    nodesRef.set (arr.push {l := 0, r := 0, cnt := n.cnt + 1})
    return arr.size
  else
    let m := (l + r) / 2
    if pos ≤ m then
      let newLeft <- update nodesRef n.l l m pos
      let arr2 <- nodesRef.get
      nodesRef.set (arr2.push {l := newLeft, r := n.r, cnt := n.cnt + 1})
      return arr2.size
    else
      let newRight <- update nodesRef n.r (m+1) r pos
      let arr2 <- nodesRef.get
      nodesRef.set (arr2.push {l := n.l, r := newRight, cnt := n.cnt + 1})
      return arr2.size

/-- query k-th smallest between two roots --/
partial def kth (nodes : Array Node) : Nat → Nat → Nat → Nat → Nat → Nat
| a, b, l, r, k =>
  if l = r then l
  else
    let leftA := nodes[a]!.l
    let leftB := nodes[b]!.l
    let cntLeft := nodes[leftA]!.cnt - nodes[leftB]!.cnt
    let m := (l + r) / 2
    if k ≤ cntLeft then
      kth nodes leftA leftB l m k
    else
      kth nodes (nodes[a]!.r) (nodes[b]!.r) (m+1) r (k - cntLeft)

/-- split input into tokens --/
def parseTokens (s : String) : Array String :=
  s.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
   |>.filter (fun t => t ≠ "")
   |> List.toArray

/-- DFS to get tin, tout and array of labels in Euler order --/
def dfsOrder (adj : Array (Array Nat)) (comp : Array Nat) (n : Nat) :
    (Array Nat × Array Nat × Array Nat) := Id.run do
  let mut tin := Array.replicate (n+1) 0
  let mut tout := Array.replicate (n+1) 0
  let mut arr := Array.replicate (n+1) 0
  let mut time := 0
  let mut stack : List (Nat × Nat × Bool) := [(1, 0, false)]
  while stack ≠ [] do
    let (u, p, vis) := stack.head!
    stack := stack.tail!
    if !vis then
      time := time + 1
      tin := tin.set! u time
      arr := arr.set! time comp[u]!
      stack := (u, p, true) :: stack
      for v in adj[u]! do
        if v ≠ p then
          stack := (v, u, false) :: stack
    else
      tout := tout.set! u time
  return (tin, tout, arr)

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := parseTokens data
  if toks.size = 0 then return
  let mut idx := 0
  let n := toks[idx]!.toNat!; idx := idx + 1
  -- labels
  let mut labels := Array.replicate (n+1) 0
  for i in [1:n+1] do
    labels := labels.set! i (toks[idx]!.toNat!)
    idx := idx + 1
  -- edges
  let mut adj : Array (Array Nat) := Array.replicate (n+1) #[]
  for _ in [0:n-1] do
    let u := toks[idx]!.toNat!
    let v := toks[idx+1]!.toNat!
    idx := idx + 2
    adj := adj.modify u (fun a => a.push v)
    adj := adj.modify v (fun a => a.push u)
  -- coordinate compression descending
  let mut pairs : Array (Nat × Nat) := Array.mkEmpty n
  for i in [1:n+1] do
    pairs := pairs.push (labels[i]!, i)
  let sorted := pairs.qsort (fun a b => a.fst > b.fst)
  let mut comp := Array.replicate (n+1) 0
  let mut rankToNode := Array.replicate (n+1) 0
  for i in [0:sorted.size] do
    let rank := i + 1
    let (_, nodeId) := sorted[i]!
    comp := comp.set! nodeId rank
    rankToNode := rankToNode.set! rank nodeId
  -- DFS to get tin/tout and euler labels
  let (tin, tout, arr) := dfsOrder adj comp n
  -- build persistent segment tree
  let nodesRef ← IO.mkRef #[{l := 0, r := 0, cnt := 0}]
  let mut roots := Array.replicate (n+1) 0
  for i in [1:n+1] do
    let newRoot ← update nodesRef roots[i-1]! 1 n arr[i]!
    roots := roots.set! i newRoot
  let nodes ← nodesRef.get
  -- queries
  let m := toks[idx]!.toNat!; idx := idx + 1
  for _ in [0:m] do
    let x := toks[idx]!.toNat!
    let k := toks[idx+1]!.toNat!
    idx := idx + 2
    let l := tin[x]!
    let r := tout[x]!
    let rank := kth nodes roots[r]! roots[l-1]! 1 n k
    let ans := rankToNode[rank]!
    IO.println ans
