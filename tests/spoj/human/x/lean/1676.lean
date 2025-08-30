/- Solution for SPOJ GEN - Text Generator
https://www.spoj.com/problems/GEN/
-/

import Std
open Std

def MOD : Nat := 10007

structure Node where
  next : Array Int
  fail : Nat
  out  : Bool
  deriving Inhabited

def newNode : Node :=
  { next := Array.mkArray 26 (-1), fail := 0, out := false }

/-- insert a word into the trie -/
def insertWord (r : IO.Ref (Array Node)) (w : String) : IO Unit := do
  let mut nodes ← r.get
  let mut state : Nat := 0
  for ch in w.data do
    let idx := ch.toNat - 'A'.toNat
    if nodes[state]!.next.get! idx == -1 then
      nodes := nodes.set! state { nodes[state]! with next := nodes[state]!.next.set! idx nodes.size }
      nodes := nodes.push newNode
      state := nodes.size - 1
    else
      state := Int.toNat (nodes[state]!.next.get! idx)
  nodes := nodes.set! state { nodes[state]! with out := true }
  r.set nodes

/-- build failure links for the Aho-Corasick automaton -/
def buildAC (r : IO.Ref (Array Node)) : IO Unit := do
  let mut nodes ← r.get
  let mut q : Array Nat := #[]
  for i in [0:26] do
    let nxt := nodes[0]!.next.get! i
    if nxt != -1 then
      let u := Int.toNat nxt
      q := q.push u
      nodes := nodes.set! u { nodes[u]! with fail := 0 }
    else
      nodes := nodes.set! 0 { nodes[0]! with next := nodes[0]!.next.set! i 0 }
  let mut head := 0
  while head < q.size do
    let v := q[head]!
    head := head + 1
    for c in [0:26] do
      let nxt := nodes[v]!.next.get! c
      if nxt != -1 then
        let u := Int.toNat nxt
        q := q.push u
        let f := nodes[v]!.fail
        let to := nodes[f]!.next.get! c
        let out := nodes[u]!.out || nodes[Int.toNat to]!.out
        nodes := nodes.set! u { nodes[u]! with fail := Int.toNat to, out := out }
      else
        let to := nodes[nodes[v]!.fail]!.next.get! c
        nodes := nodes.set! v { nodes[v]! with next := nodes[v]!.next.set! c to }
  r.set nodes

/-- construct adjacency matrix ignoring transitions to terminal states -/
def buildAdj (nodes : Array Node) : Array (Array Nat) :=
  let m := nodes.size
  Id.run do
    let mut mat : Array (Array Nat) := Array.mkArray m (Array.mkArray m 0)
    for i in [0:m] do
      for c in [0:26] do
        let j := Int.toNat (nodes[i]!.next.get! c)
        if !nodes[j]!.out then
          let row := mat.get! i
          mat := mat.set! i (row.set! j ((row.get! j + 1) % MOD))
    return mat

/-- matrix multiplication modulo MOD -/
def matMul (A B : Array (Array Nat)) : Array (Array Nat) :=
  let n := A.size
  Id.run do
    let mut C : Array (Array Nat) := Array.mkArray n (Array.mkArray n 0)
    for i in [0:n] do
      for j in [0:n] do
        let mut s : Nat := 0
        for k in [0:n] do
          s := (s + (A.get! i).get! k * (B.get! k).get! j) % MOD
        let row := C.get! i
        C := C.set! i (row.set! j s)
    return C

/-- matrix exponentiation modulo MOD -/
def matPow (M : Array (Array Nat)) (p : Nat) : Array (Array Nat) :=
  let n := M.size
  let mut base := M
  let mut exp := p
  let mut res := Id.run do
    let mut I : Array (Array Nat) := Array.mkArray n (Array.mkArray n 0)
    for i in [0:n] do
      I := I.set! i ((I.get! i).set! i 1)
    return I
  while exp > 0 do
    if exp % 2 = 1 then
      res := matMul res base
    base := matMul base base
    exp := exp / 2
  res

/-- fast exponentiation modulo MOD -/
def powMod (b e : Nat) : Nat :=
  Id.run do
    let mut res : Nat := 1
    let mut base := b % MOD
    let mut exp := e
    while exp > 0 do
      if exp % 2 = 1 then
        res := (res * base) % MOD
      base := (base * base) % MOD
      exp := exp / 2
    return res

/-- process tokenized input recursively -/
partial def solve (toks : Array String) (idx : Nat) : IO Unit := do
  if h : idx < toks.size then
    let n := toks[idx]!.toNat!
    let l := toks[idx+1]!.toNat!
    let mut i := idx + 2
    let r ← IO.mkRef (#[newNode])
    for _ in [0:n] do
      let w := toks[i]!
      i := i + 1
      insertWord r w
    buildAC r
    let nodes ← r.get
    let adj := buildAdj nodes
    let mat := matPow adj l
    let row0 := mat[0]!
    let safe := row0.foldl (fun s x => (s + x) % MOD) 0
    let total := powMod 26 l
    let ans := (total + MOD - safe) % MOD
    IO.println ans
    solve toks i
  else
    pure ()

/-- main entry point -/
def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "")
                |> List.toArray
  solve toks 0
