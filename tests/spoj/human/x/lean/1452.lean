/- Solution for SPOJ CAKE - Birthday Cake
https://www.spoj.com/problems/CAKE/
-/

import Std
open Std

-- integer square root via linear scan (sufficient for N ≤ 10000)
partial def isqrtAux (n i : Nat) : Nat :=
  if (i+1)*(i+1) > n then i else isqrtAux n (i+1)

def isqrt (n : Nat) : Nat := isqrtAux n 0

def maxM : Nat := 10

-- minimal possible total volume for k layers
def minVol : Array Nat := Id.run do
  let mut arr := Array.mkEmpty (maxM+1)
  arr := arr.push 0
  for i in [1:maxM+1] do
    let prev := arr[i-1]!
    arr := arr.push (prev + i*i*i)
  return arr

-- minimal possible side area for k layers
def minArea : Array Nat := Id.run do
  let mut arr := Array.mkEmpty (maxM+1)
  arr := arr.push 0
  for i in [1:maxM+1] do
    let prev := arr[i-1]!
    arr := arr.push (prev + 2*i*i)
  return arr

partial def dfs (k vol area maxR maxH best total : Nat) : Nat :=
  if k == 0 then
    if vol == 0 && area < best then area else best
  else if vol < minVol[k]! then
    best
  else if area + minArea[k]! >= best then
    best
  else
    let boundV := vol - minVol[k-1]!
    let rUpper0 := isqrt (boundV / k)
    let rUpper := if maxR - 1 < rUpper0 then maxR - 1 else rUpper0
    let rec loopR (r : Nat) (best : Nat) : Nat :=
      if r < k then best else
        let hUpper0 := boundV / (r*r)
        let hUpper := if maxH - 1 < hUpper0 then maxH - 1 else hUpper0
        let rec loopH (h : Nat) (best : Nat) : Nat :=
          if h < k then best else
            let volHere := r*r*h
            if volHere > vol then
              loopH (h-1) best
            else if vol - volHere < minVol[k-1]! then
              loopH (h-1) best
            else
              let areaHere := area + 2*r*h + (if k == total then r*r else 0)
              if areaHere + minArea[k-1]! >= best then
                loopH (h-1) best
              else
                let best := dfs (k-1) (vol - volHere) areaHere r h best total
                loopH (h-1) best
        let best := loopH hUpper best
        loopR (r-1) best
    loopR rUpper best

def solveCase (N M : Nat) : Nat :=
  let best := dfs M N 0 (isqrt N + 1) (N + 1) 1000000000 M
  if best == 1000000000 then 0 else best

partial def loopCases (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  for _ in [0:t] do
    let nLine ← h.getLine
    let mLine ← h.getLine
    let N := nLine.trim.toNat!
    let M := mLine.trim.toNat!
    IO.println (solveCase N M)

def main : IO Unit := do
  let h ← IO.getStdin
  let tLine ← h.getLine
  let T := tLine.trim.toNat!
  loopCases h T
