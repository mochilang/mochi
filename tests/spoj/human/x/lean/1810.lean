/- Solution for SPOJ ORZ - Nuclear Plants
https://www.spoj.com/problems/ORZ/
-/

import Std
open Std

structure Circle where
  x : Float
  y : Float
  r : Float

def rSmall : Float := 0.58

def rLarge : Float := 1.31

-- coverage along vertical line x
partial def coverage (m : Float) (cs : Array Circle) (x : Float) : Float :=
  let intervals := cs.foldl (init := []) fun acc c =>
    let dx := (x - c.x).abs
    if dx <= c.r then
      let dy := Float.sqrt (c.r * c.r - dx * dx)
      let y1 := Float.max 0.0 (c.y - dy)
      let y2 := Float.min m (c.y + dy)
      if y1 < y2 then (y1, y2) :: acc else acc
    else acc
  match intervals with
  | [] => 0.0
  | _ =>
    let sorted := intervals.qsort (fun a b => a.fst < b.fst)
    let rec merge (cur : Float × Float) (rest : List (Float × Float)) (acc : Float) : Float :=
      match rest with
      | [] => acc + (cur.snd - cur.fst)
      | (s,e)::rs =>
        if s <= cur.snd then
          merge (cur.fst, Float.max cur.snd e) rs acc
        else
          merge (s,e) rs (acc + (cur.snd - cur.fst))
    match sorted with
    | [] => 0.0
    | (s,e)::rs => merge (s,e) rs 0.0

-- adaptive Simpson integration of coverage from a to b
partial def adapt (cov : Float → Float) (a b eps fa fm fb : Float) : Float :=
  let m := (a + b) / 2.0
  let lm := (a + m) / 2.0
  let rm := (m + b) / 2.0
  let fl := cov lm
  let fr := cov rm
  let left := (fa + 4.0 * fl + fm) * (m - a) / 6.0
  let right := (fm + 4.0 * fr + fb) * (b - m) / 6.0
  let whole := (fa + 4.0 * fm + fb) * (b - a) / 6.0
  if (left + right - whole).abs <= 15.0 * eps then
    left + right + (left + right - whole) / 15.0
  else
    adapt cov a m (eps / 2.0) fa fl fm +
    adapt cov m b (eps / 2.0) fm fr fb

def integrate (m : Float) (cs : Array Circle) (a b : Float) : Float :=
  let cov := coverage m cs
  let fa := cov a
  let fb := cov b
  let fm := cov ((a + b) / 2.0)
  adapt cov a b 1e-4 fa fm fb

-- format float with two decimals
def format2 (x : Float) : String :=
  let y := x + 0.005
  let s := y.toString
  let parts := s.splitOn "."
  let intPart := parts.get! 0
  let fracPart :=
    if h : parts.length > 1 then
      let f := parts.get! 1
      (f ++ "00").take 2
    else
      "00"
  intPart ++ "." ++ fracPart

partial def readCircles (toks : Array String) (idx cnt : Nat) (r : Float)
    (cs : Array Circle) : (Array Circle × Nat) :=
  if cnt = 0 then (cs, idx) else
    let x := toks.get! idx |>.toFloat!
    let y := toks.get! (idx+1) |>.toFloat!
    readCircles toks (idx+2) (cnt-1) r (cs.push {x:=x, y:=y, r:=r})

partial def process (toks : Array String) (idx : Nat) (acc : Array String) : Array String :=
  if h : idx < toks.size then
    let n := toks.get! idx |>.toNat!
    let m := toks.get! (idx+1) |>.toNat!
    let ks := toks.get! (idx+2) |>.toNat!
    let kl := toks.get! (idx+3) |>.toNat!
    if n = 0 && m = 0 && ks = 0 && kl = 0 then
      acc
    else
      let idx := idx + 4
      let (cs, idx) := readCircles toks idx ks rSmall #[]
      let (cs, idx) := readCircles toks idx kl rLarge cs
      let nF := Float.ofNat n
      let mF := Float.ofNat m
      let minX := cs.foldl (init := nF) (fun a c => Float.min a (Float.max 0.0 (c.x - c.r)))
      let maxX := cs.foldl (init := 0.0) (fun a c => Float.max a (Float.min nF (c.x + c.r)))
      let forbidden :=
        if cs.isEmpty || minX >= maxX then
          0.0
        else
          integrate mF cs minX maxX
      let area := nF * mF - forbidden
      process toks idx (acc.push (format2 area))
  else
    acc

def main : IO Unit := do
  let data ← (← IO.getStdin).readToEnd
  let toks := data.toString.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r') |>.filter (· ≠ "") |>.toArray
  let res := process toks 0 #[]
  for line in res do
    IO.println line
