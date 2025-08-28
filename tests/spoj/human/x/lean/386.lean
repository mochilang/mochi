/- Solution for SPOJ HELP - Help the problem setter
https://www.spoj.com/problems/HELP/
-/

import Std
open Std

structure Node where
  left : Int
  right : Int

def readInts : IO (Array Int) := do
  let s ← IO.readToEnd (← IO.getStdin)
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  let mut arr : Array Int := #[]
  for p in parts do
    if p.length > 0 then
      arr := arr.push p.toInt!
  return arr

partial def buildFreq (nodes : Array Node) (root : Nat) : Array Nat :=
  Id.run do
    let n := nodes.size - 1
    let freqRef ← ST.mkRef (Array.mkArray (n+1) 0)
    let rec go (i : Nat) : ST _ Nat := do
      let fArr ← freqRef.get
      let cur := fArr.get! i
      if cur != 0 then
        return cur
      let node := nodes.get! i
      let lFreq ← if node.left = -1 then pure 0 else go node.left.toNat
      let rFreq ← if node.right = -1 then pure 0 else go node.right.toNat
      let f :=
        if node.left = -1 && node.right = -1 then 1
        else if node.left = -1 then 2 * rFreq
        else if node.right = -1 then 2 * lFreq
        else Nat.max lFreq rFreq
      freqRef.modify (fun arr => arr.set! i f)
      return f
    _ ← go root
    freqRef.get

partial def process (data : Array Int) : String :=
  Id.run do
    let mut idx := 0
    let mut out : Array String := #[]
    while idx < data.size do
      let n := data.get! idx
      idx := idx + 1
      if n = 0 then
        break
      let nNat := n.toNat!
      let mut nodes : Array Node := Array.mkArray (nNat + 1) {left := 0, right := 0}
      let mut child : Array Bool := Array.mkArray (nNat + 1) false
      for i in [1:nNat+1] do
        let l := data.get! idx; idx := idx + 1
        let r := data.get! idx; idx := idx + 1
        nodes := nodes.set! i {left := l, right := r}
        if l != -1 then child := child.set! l.toNat true
        if r != -1 then child := child.set! r.toNat true
      let mut root := 1
      for i in [1:nNat+1] do
        if (child.get! i) = false then
          root := i
      let freqs := buildFreq nodes root
      let mut parts : Array String := #[]
      for i in [1:nNat+1] do
        parts := parts.push (toString (freqs.get! i))
      out := out.push (String.intercalate " " parts.toList)
    return String.intercalate "\n" out.toList

def main : IO Unit := do
  let data ← readInts
  IO.println (process data)
