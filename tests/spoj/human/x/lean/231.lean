/- Solution for SPOJ ZEBRA - The Zebra Crossing
https://www.spoj.com/problems/ZEBRA/
-/
import Std
open Std

structure Ped where
  start : Float
  finish : Float

def tanDeg60 (a : Int) : Float :=
  let rad := (Float.ofInt a) / 60.0 * (Float.pi / 180.0)
  Float.tan rad

-- compute maximum overlap for intervals (open at start point)
def maxOverlap (arr : Array Ped) : Nat :=
  let mut events : Array (Float × Int) := Array.empty
  for p in arr do
    let s := Float.min p.start p.finish
    let e := Float.max p.start p.finish
    -- open at s, closed at e
    events := events.push (s + 1e-7, 1)
    events := events.push (e, -1)
  events := events.qsort (fun a b => a.1 < b.1)
  let mut cur : Int := 0
  let mut best : Int := 0
  for e in events do
    cur := cur + e.2
    if cur > best then
      best := cur
  best.toNat

partial def solve (toks : Array String) (idx : Nat) (cases : Nat) (acc : List Nat) : List Nat :=
  if cases == 0 then acc.reverse else
    let n := toks.get! idx |>.toNat!
    let w := toks.get! (idx+1) |>.toNat!
    let v := toks.get! (idx+2) |>.toNat!
    let mut j := idx + 3
    let mut mp : Std.HashMap Int (Array Ped) := {}
    for _ in [0:n] do
      let xi := toks.get! j |>.toInt!
      let ti := toks.get! (j+1) |>.toInt!
      let ai := toks.get! (j+2) |>.toInt!
      j := j + 3
      if w > v * ti then
        ()
      else
        let slope := tanDeg60 ai
        let finish := Float.ofInt xi + Float.ofNat w * slope
        let ped := { start := Float.ofInt xi, finish := finish }
        let arr := mp.findD ti #[]
        mp := mp.insert ti (arr.push ped)
    let mut ans : Nat := 0
    for entry in mp.toList do
      let t := entry.fst
      let arr := entry.snd
      if v * t == w then
        let c := maxOverlap arr
        if c > ans then ans := c
      else if v * t > w then
        let c := arr.size
        if c > ans then ans := c
    solve toks j (cases - 1) (ans :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                  |>.filter (fun s => s ≠ "") |> Array.ofList
  let t := toks.get! 0 |>.toNat!
  let outs := solve toks 1 t []
  for o in outs do
    IO.println o
