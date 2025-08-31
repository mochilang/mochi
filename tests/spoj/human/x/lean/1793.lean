/- Solution for SPOJ GEN2 - Text Generater II
https://www.spoj.com/problems/GEN2/
-/

import Std
open Std

-- helper to map character to index 0..25
def charIdx (c : Char) : Nat := c.toNat - 'a'.toNat

structure Node where
  next : Array Int
  fail : Nat
  mask : Nat
  bad  : Bool
  deriving Inhabited

def mkNode : Node :=
  { next := Array.replicate 26 (-1), fail := 0, mask := 0, bad := false }

-- insert pattern into trie
partial def insert (nodes : Array Node) (s : List Char) (m : Nat) (badFlag : Bool) : Array Node :=
  Id.run do
    let mut nodes := nodes
    let mut cur : Nat := 0
    for c in s do
      let idx := charIdx c
      let nxt := nodes[cur]!.next[idx]!
      if nxt = -1 then
        nodes := nodes.push mkNode
        let newIdx := nodes.size - 1
        nodes := nodes.modify cur (fun n => { n with next := n.next.set! idx (Int.ofNat newIdx) })
        cur := newIdx
      else
        cur := Int.toNat nxt
    nodes := nodes.modify cur (fun n => { n with mask := Nat.lor n.mask m, bad := n.bad || badFlag })
    return nodes

-- build automaton failure links
partial def buildAutomaton (nodes0 : Array Node) : Array Node :=
  Id.run do
    let mut nodes := nodes0
    let mut q : List Nat := []
    -- initialize root transitions
    for c in [0:26] do
      let nxt := nodes[0]!.next[c]!
      if nxt = -1 then
        nodes := nodes.modify 0 (fun n => { n with next := n.next.set! c 0 })
      else
        let ni := Int.toNat nxt
        nodes := nodes.modify ni (fun n => { n with fail := 0 })
        q := q.concat [ni]
    -- BFS
    while q ≠ [] do
      let v := q.head!
      q := q.tail!
      for c in [0:26] do
        let nxt := nodes[v]!.next[c]!
        if nxt = -1 then
          let f := nodes[v]!.fail
          let nf := nodes[f]!.next[c]!
          nodes := nodes.modify v (fun n => { n with next := n.next.set! c nf })
        else
          let u := Int.toNat nxt
          let f := nodes[v]!.fail
          let nf := nodes[f]!.next[c]!
          let nfNat := Int.toNat nf
          nodes := nodes.modify u (fun n =>
            { n with fail := nfNat,
                    mask := Nat.lor n.mask (nodes[nfNat]!.mask),
                    bad  := n.bad || nodes[nfNat]!.bad })
          q := q.concat [u]
    return nodes

-- build transition matrix of size (states*8)
partial def buildMatrix (nodes : Array Node) (modv : Nat) : Array (Array Nat) :=
  let S := nodes.size
  let size := S * 8
  Id.run do
    let mut mat : Array (Array Nat) := Array.replicate size (Array.replicate size 0)
    for s in [0:S] do
      if ¬ nodes[s]!.bad then
        for m in [0:8] do
          let from := s * 8 + m
          for c in [0:26] do
            let ns := Int.toNat (nodes[s]!.next[c]!)
            if ¬ nodes[ns]!.bad then
              let nm := Nat.lor m (nodes[ns]!.mask)
              let to := ns * 8 + nm
              mat := mat.modify to (fun row => row.set! from ((row[from]! + 1) % modv))
    return mat

-- matrix multiplication
partial def matMul (A B : Array (Array Nat)) (modv : Nat) : Array (Array Nat) :=
  Id.run do
    let n := A.size
    let mut C : Array (Array Nat) := Array.replicate n (Array.replicate n 0)
    for i in [0:n] do
      for k in [0:n] do
        let aik := A[i]![k]!
        if aik ≠ 0 then
          for j in [0:n] do
            let val := (C[i]![j]! + aik * B[k]![j]!) % modv
            C := C.modify i (fun row => row.set! j val)
    return C

-- identity matrix
def matId (n : Nat) : Array (Array Nat) := Id.run do
  let mut I := Array.replicate n (Array.replicate n 0)
  for i in [0:n] do
    I := I.modify i (fun row => row.set! i 1)
  return I

-- matrix exponentiation
partial def matPow (M : Array (Array Nat)) (n : Nat) (modv : Nat) : Array (Array Nat) :=
  if n = 0 then matId M.size
  else if n = 1 then M
  else if n % 2 = 0 then
    let half := matPow M (n / 2) modv
    matMul half half modv
  else
    matMul M (matPow M (n-1) modv) modv

-- multiply matrix by vector
partial def mulVec (M : Array (Array Nat)) (v : Array Nat) (modv : Nat) : Array Nat :=
  Id.run do
    let n := M.size
    let mut res := Array.replicate n 0
    for i in [0:n] do
      let row := M[i]!
      let mut s : Nat := 0
      for j in [0:n] do
        s := (s + row[j]! * v[j]!) % modv
      res := res.set! i s
    return res

-- solve single test case
partial def solve (L : Nat) (evils : List String) : Nat :=
  let modv := 8000
  let required : List (String × Nat) := [("hl",1), ("hj",2), ("fgd",4)]
  let mut nodes : Array Node := #[mkNode]
  -- insert required patterns
  for (s,m) in required do
    nodes := insert nodes s.data m false
  -- insert evil patterns
  for e in evils do
    nodes := insert nodes e.data 0 true
  let nodes := buildAutomaton nodes
  let mat := buildMatrix nodes modv
  let pow := matPow mat L modv
  let size := nodes.size * 8
  let mut vec := Array.replicate size 0
  vec := vec.set! 0 1 -- start at root with mask 0
  let resVec := mulVec pow vec modv
  let mut total : Nat := 0
  for s in [0:nodes.size] do
    let idx := s * 8 + 7
    total := (total + resVec[idx]!) % modv
  return (total / 8) % 1000

partial def parseAll (toks : Array String) (idx : Nat) (acc : List Nat) : List Nat :=
  if h : idx < toks.size then
    let L := (toks[idx]!).toNat!
    let N := (toks[idx+1]!).toNat!
    let mut evs : List String := []
    let mut j : Nat := 0
    while _ : j < N do
      evs := evs.concat (toks[idx+2+j]!);
      j := j + 1
    let res := solve L evs
    parseAll toks (idx + 2 + N) (res :: acc)
  else
    acc.reverse

def main : IO Unit := do
  let stdin ← IO.getStdin
  let data ← stdin.readToEnd
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (fun s => s ≠ "") |> List.toArray
  let outputs := parseAll toks 0 []
  for o in outputs do
    IO.println (toString o)
