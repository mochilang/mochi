/- Solution for SPOJ RAILROAD - Railroads
https://www.spoj.com/problems/RAILROAD/
-/

import Std
open Std

structure Edge where
  dep : Nat
  arr : Nat
  to  : Nat

structure Node where
  key : Nat
  arr : Nat
  dep : Nat
  city : Nat

structure MinHeap where
  data : Array Node

namespace MinHeap
instance : Inhabited MinHeap := ⟨⟨#[]⟩⟩

def empty : MinHeap := ⟨#[]⟩

private def swap! (a : Array Node) (i j : Nat) : Array Node := Id.run do
  let tmp := a.get! i
  let a := a.set! i (a.get! j)
  let a := a.set! j tmp
  return a

partial def push (h : MinHeap) (v : Node) : MinHeap := Id.run do
  let mut arr := h.data.push v
  let mut i := arr.size - 1
  while i > 0 do
    let p := (i - 1) / 2
    let vi := arr.get! i
    let vp := arr.get! p
    if vi.key < vp.key then
      arr := arr.set! i vp
      arr := arr.set! p vi
      i := p
    else
      break
  return ⟨arr⟩

partial def pop? (h : MinHeap) : Option (Node × MinHeap) :=
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
        let l := 2*i + 1
        if l ≥ size then break
        let r := l + 1
        let c := if r < size && (arr.get! r).key < (arr.get! l).key then r else l
        if (arr.get! c).key < (arr.get! i).key then
          arr := swap! arr i c
          i := c
        else
          break
      some (top, ⟨arr⟩)
end MinHeap

def key (arr dep : Nat) : Nat :=
  arr * 10000 + (9999 - dep)

def timeStr (t : Nat) : String :=
  let hh := t / 100
  let mm := t % 100
  let pad := fun n => if n < 10 then "0" ++ toString n else toString n
  pad hh ++ pad mm

def dijkstra (n : Nat) (adj : Array (Array Edge))
    (start dest startTime : Nat) : Option (Nat × Nat) :=
  Id.run do
    let inf := 10000
    let mut bestArr : Array Nat := Array.mkArray n inf
    let mut bestDep : Array Nat := Array.mkArray n 0
    bestArr := bestArr.set! start startTime
    bestDep := bestDep.set! start startTime
    let mut pq : MinHeap := MinHeap.empty
    pq := MinHeap.push pq {key := key startTime startTime, arr := startTime, dep := startTime, city := start}
    while pq.data.size > 0 do
      let some (node, pq1) := MinHeap.pop? pq | break
      pq := pq1
      let arr := node.arr
      let dep := node.dep
      let u := node.city
      let ba := bestArr.get! u
      let bd := bestDep.get! u
      if arr > ba || (arr = ba && dep < bd) then
        continue
      for e in adj.get! u do
        if e.dep >= arr then
          let nd := if u = start && arr = startTime then e.dep else dep
          let na := e.arr
          let ba2 := bestArr.get! e.to
          let bd2 := bestDep.get! e.to
          if na < ba2 || (na = ba2 && nd > bd2) then
            bestArr := bestArr.set! e.to na
            bestDep := bestDep.set! e.to nd
            pq := MinHeap.push pq {key := key na nd, arr := na, dep := nd, city := e.to}
    let finalArr := bestArr.get! dest
    if finalArr = inf then none
    else some (bestDep.get! dest, finalArr)

partial def solve (tokens : Array String) (startIdx sc rem : Nat) : IO Unit := do
  if rem = 0 then
    pure ()
  else
    let mut idx := startIdx
    let c := tokens.get! idx |>.toNat!
    idx := idx + 1
    let mut map : Std.HashMap String Nat := {}
    let mut names : Array String := #[]
    for i in [0:c] do
      let name := tokens.get! idx
      idx := idx + 1
      map := map.insert name i
      names := names.push name
    let t := tokens.get! idx |>.toNat!
    idx := idx + 1
    let mut adj : Array (Array Edge) := Array.mkArray c #[]
    for _ in [0:t] do
      let ti := tokens.get! idx |>.toNat!
      idx := idx + 1
      let mut prevCity? : Option Nat := none
      let mut prevTime := 0
      for _ in [0:ti] do
        let time := tokens.get! idx |>.toNat!
        let cityName := tokens.get! (idx+1)
        idx := idx + 2
        let city := map.find! cityName
        match prevCity? with
        | some pc =>
            adj := adj.modify pc (fun a => a.push {dep := prevTime, arr := time, to := city})
            prevCity? := some city
            prevTime := time
        | none =>
            prevCity? := some city
            prevTime := time
    let startTime := tokens.get! idx |>.toNat!
    let sName := tokens.get! (idx+1)
    let dName := tokens.get! (idx+2)
    idx := idx + 3
    let start := map.find! sName
    let dest := map.find! dName
    IO.println s!"Scenario {sc}"
    match dijkstra c adj start dest startTime with
    | some (dep, arr) =>
        IO.println s!"Departure {timeStr dep} {sName}"
        IO.println s!"Arrival   {timeStr arr} {dName}"
    | none =>
        IO.println "No connection"
    IO.println ""
    solve tokens idx (sc+1) (rem-1)

def main : IO Unit := do
  let data ← IO.readStdin
  let tokens := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r') |>.filter (fun s => s ≠ "")
  let t := tokens.get! 0 |>.toNat!
  solve tokens 1 1 t
