/- Solution for SPOJ COPER - Crane Operator
https://www.spoj.com/problems/COPER/
-/

import Std
open Std

partial def solveCase (N M q p m d : Nat) : Nat :=
  Id.run do
    let mut used := Array.replicate N false
    used := used.set! M true
    let mut posArr := Array.replicate N 0
    let mut c := 0
    let mut i := 1
    while h : i < N do
      c := (c * q + p) % m
      let mut found := false
      let mut y := 0
      while h1 : found = false do
        let mut x := 0
        while hx : x < N do
          let pos := (c + d * x + y) % N
          if pos != M && !used.get! pos then
            posArr := posArr.set! i pos
            used := used.set! pos true
            found := true
            x := N
          else
            x := x + 1
        if found = false then
          y := y + 1
      i := i + 1
    let mut perm := Array.replicate N 0
    perm := perm.set! 0 M
    for j in [1:N] do
      perm := perm.set! j (posArr.get! j)
    let mut vis := Array.replicate N false
    let mut moves := 0
    for i in [0:N] do
      if !vis.get! i then
        let mut cur := i
        let mut len := 0
        let mut has0 := false
        while h : vis.get! cur = false do
          vis := vis.set! cur true
          if cur = 0 then
            has0 := true
          cur := perm.get! cur
          len := len + 1
        if len > 1 then
          if has0 then
            moves := moves + (len - 1)
          else
            moves := moves + (len + 1)
    return moves

partial def loop (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let line ← h.getLine
    let parts := line.trim.split (· = ' ') |>.filter (· ≠ "")
    let N := parts[0]! |>.toNat!
    let M := parts[1]! |>.toNat!
    let q := parts[2]! |>.toNat!
    let p := parts[3]! |>.toNat!
    let m := parts[4]! |>.toNat!
    let d := parts[5]! |>.toNat!
    IO.println (solveCase N M q p m d)
    loop h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
