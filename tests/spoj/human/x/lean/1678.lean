/- Solution for SPOJ TREASURY - Royal Treasury
https://www.spoj.com/problems/TREASURY/
-/

import Std
open Std

structure Info where
  s0 : Nat
  w0 : Nat
  s1 : Nat
  w1 : Nat
  msize : Nat
  wchoice : Nat

partial def dfs (adj : Array (List Nat)) (u : Nat) : ((Nat × Nat) × (Nat × Nat)) :=
  let children := adj.get! u
  let infos := children.map (fun v =>
    let ((s0, w0), (s1, w1)) := dfs adj v
    let msize := max s0 s1
    let wchoice :=
      if s0 > s1 then w0
      else if s0 < s1 then w1
      else w0 + w1
    Info.mk s0 w0 s1 w1 msize wchoice)

  let size1 := infos.foldl (fun acc i => acc + i.s0) 0
  let ways1 := infos.foldl (fun acc i => acc * i.w0) 1

  let baseSize := infos.foldl (fun acc i => acc + i.msize) 0
  let baseWays := infos.foldl (fun acc i => acc * i.wchoice) 1

  let (bestSize, bestWays) :=
    infos.foldl (fun (best : Nat × Nat) i =>
      let (bs, bw) := best
      if i.w1 > 0 then
        let size := 1 + i.s1 + (baseSize - i.msize)
        let ways := i.w1 * (baseWays / i.wchoice)
        if size > bs then (size, ways)
        else if size == bs then (bs, bw + ways)
        else best
      else best) (baseSize, baseWays)

  ((bestSize, bestWays), (size1, ways1))

partial def build (i : Nat) (tok : List String) (adj : Array (List Nat)) : (Array (List Nat) × List String) :=
  match i with
  | 0 => (adj, tok)
  | n+1 =>
    match tok with
    | idStr :: kStr :: rest =>
      let id := idStr.toNat!
      let k := kStr.toNat!
      let (subsStr, rest2) := rest.splitAt k
      let subs := subsStr.map String.toNat!
      let adj := adj.set! id subs
      build n rest2 adj
    | _ => (adj, [])

partial def parseCases : List String → List (Array (List Nat))
  | [] => []
  | nStr :: rest =>
    let n := nStr.toNat!
    let (adj, rest2) := build n rest (Array.mkArray (n + 1) [])
    adj :: parseCases rest2

def main : IO Unit := do
  let data ← IO.readStdin
  let tokens := data.split (· ≤ ' ') |>.filter (· ≠ "")
  for adj in parseCases tokens do
    let ((s, w), _) := dfs adj 1
    IO.println s
    IO.println w
