/- Solution for SPOJ CONNECT - Connections
https://www.spoj.com/problems/CONNECT/
-/

import Std
open Std

structure Query where
  src : Nat
  dst : Nat
  k   : Nat

structure MinHeap where
  data : Array (Nat × Nat) -- (distance, vertex)

namespace MinHeap

instance : Inhabited MinHeap := ⟨⟨#[]⟩⟩

def empty : MinHeap := ⟨#[]⟩

private def swap! {α} (a : Array α) (i j : Nat) : Array α := Id.run do
  let tmp := a.get! i
  let a := a.set! i (a.get! j)
  let a := a.set! j tmp
  return a

partial def push (h : MinHeap) (p : Nat × Nat) : MinHeap := Id.run do
  let mut arr := h.data.push p
  let mut i := arr.size - 1
  while i > 0 do
    let parent := (i - 1) / 2
    let vi := arr.get! i
    let vp := arr.get! parent
    if vi.fst < vp.fst then
      arr := arr.set! i vp
      arr := arr.set! parent vi
      i := parent
    else
      break
  return ⟨arr⟩

partial def pop? (h : MinHeap) : Option ((Nat × Nat) × MinHeap) :=
  if h.data.isEmpty then none else
    let top := h.data.get! 0
    let size := h.data.size
    if size = 1 then
      some (top, ⟨#[]⟩)
    else
      let mut arr := h.data.set! 0 (h.data.get! (size-1))
      arr := arr.pop
      let mut i := 0
      let size := arr.size
      while true do
        let left := 2*i+1
        if left ≥ size then break
        let right := left+1
        let c := if right < size && (arr.get! right).fst < (arr.get! left).fst then right else left
        if (arr.get! c).fst < (arr.get! i).fst then
          arr := swap! arr i c
          i := c
        else
          break
      some (top, ⟨arr⟩)

end MinHeap

partial def kShortestFrom (n : Nat) (adj : Array (Array (Nat × Nat))) (src maxk : Nat) : Array (Array Nat) :=
  Id.run do
    let mut cnt : Array Nat := Array.mkArray (n+1) 0
    let mut res : Array (Array Nat) := Array.mkArray (n+1) #[]
    let mut pq : MinHeap := MinHeap.empty
    pq := MinHeap.push pq (0, src)
    while pq.data.size > 0 do
      let some ((dist,u), pq1) := MinHeap.pop? pq | break
      pq := pq1
      if cnt.get! u < maxk then
        cnt := cnt.set! u (cnt.get! u + 1)
        res := res.modify u (fun a => a.push dist)
        for (v,w) in adj.get! u do
          if cnt.get! v < maxk then
            pq := MinHeap.push pq (dist + w, v)
    return res

partial def solveCase (n : Nat) (adj : Array (Array (Nat × Nat))) (qs : Array Query) : Array String :=
  Id.run do
    let mut need : Std.HashMap Nat Nat := {}
    for q in qs do
      let v := need.findD q.src 0
      if q.k > v then
        need := need.insert q.src q.k
    let mut table : Array (Array (Array Nat)) := Array.mkArray (n+1) #[]
    for (src,maxk) in need.toList do
      let res := kShortestFrom n adj src maxk
      table := table.set! src res
    let mut outs : Array String := #[]
    for q in qs do
      let arr := table.get! q.src |>.get! q.dst
      let ans := if arr.size ≥ q.k then toString (arr.get! (q.k-1)) else "-1"
      outs := outs.push ans
    return outs

def solve (toks : Array String) : Array (Array String) :=
  Id.run do
    let t := toks.get! 0 |>.toNat!
    let mut idx := 1
    let mut res : Array (Array String) := #[]
    for _ in [0:t] do
      let n := toks.get! idx |>.toNat!
      let m := toks.get! (idx+1) |>.toNat!
      idx := idx + 2
      let mut adj : Array (Array (Nat × Nat)) := Array.mkArray (n+1) #[]
      for _ in [0:m] do
        let a := toks.get! idx |>.toNat!
        let b := toks.get! (idx+1) |>.toNat!
        let w := toks.get! (idx+2) |>.toNat!
        idx := idx + 3
        adj := adj.modify a (fun arr => arr.push (b,w))
      let q := toks.get! idx |>.toNat!
      idx := idx + 1
      let mut qs : Array Query := #[]
      for _ in [0:q] do
        let c := toks.get! idx |>.toNat!
        let d := toks.get! (idx+1) |>.toNat!
        let k := toks.get! (idx+2) |>.toNat!
        idx := idx + 3
        qs := qs.push {src:=c, dst:=d, k:=k}
      let outs := solveCase n adj qs
      res := res.push outs
    return res

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
                |>.filter (fun s => s ≠ "")
                |> Array.ofList
  let results := solve toks
  for i in [0:results.size] do
    for line in results.get! i do
      IO.println line
    if i + 1 < results.size then
      IO.println ""
  return ()
