/- Solution for SPOJ UCUBE - The Unstable Cube
https://www.spoj.com/problems/UCUBE/
-/

import Std
open Std

/-- Set cube[x][y][z] := v. --/
def cubeSet (cube : Array (Array (Array Char))) (x y z : Nat) (v : Char) :
    Array (Array (Array Char)) :=
  let layer := cube[x]!
  let row := layer[y]!
  let row := row.set! z v
  let layer := layer.set! y row
  cube.set! x layer

/-- Get cube[x][y][z]. --/
def cubeGet (cube : Array (Array (Array Char))) (x y z : Nat) : Char :=
  cube[x]![y]![z]!

/-- Process one test case. --/
def solveCase (n : Nat) (front leftv back rightv topv bottom :
    Array (Array Char)) : Nat := Id.run do
  let mut cube : Array (Array (Array Char)) :=
    Array.replicate n (Array.replicate n (Array.replicate n '?'))
  -- front view
  for y in [0:n] do
    let row := front[y]!
    for x in [0:n] do
      let c := row[x]!
      if c = '.' then
        for z in [0:n] do
          cube := cubeSet cube x y z '.'
      else
        let mut z := 0
        while z < n do
          let cell := cubeGet cube x y z
          if cell = '.' then
            z := z + 1
          else if cell = '?' ∨ cell = c then
            cube := cubeSet cube x y z c
            z := n
          else
            cube := cubeSet cube x y z '.'
            z := z + 1
  -- left view
  for y in [0:n] do
    let row := leftv[y]!
    for j in [0:n] do
      let c := row[j]!
      let z := n - 1 - j
      if c = '.' then
        for x in [0:n] do
          cube := cubeSet cube x y z '.'
      else
        let mut x := 0
        while x < n do
          let cell := cubeGet cube x y z
          if cell = '.' then
            x := x + 1
          else if cell = '?' ∨ cell = c then
            cube := cubeSet cube x y z c
            x := n
          else
            cube := cubeSet cube x y z '.'
            x := x + 1
  -- back view
  for y in [0:n] do
    let row := back[y]!
    for j in [0:n] do
      let c := row[j]!
      let x := n - 1 - j
      if c = '.' then
        for z in [0:n] do
          cube := cubeSet cube x y z '.'
      else
        let mut z := n
        while z > 0 do
          let idx := z - 1
          let cell := cubeGet cube x y idx
          if cell = '.' then
            z := idx
          else if cell = '?' ∨ cell = c then
            cube := cubeSet cube x y idx c
            z := 0
          else
            cube := cubeSet cube x y idx '.'
            z := idx
  -- right view
  for y in [0:n] do
    let row := rightv[y]!
    for j in [0:n] do
      let c := row[j]!
      let z := j
      if c = '.' then
        for x in [0:n] do
          cube := cubeSet cube (n - 1 - x) y z '.'
      else
        let mut x := n
        while x > 0 do
          let idx := x - 1
          let cell := cubeGet cube idx y z
          if cell = '.' then
            x := idx
          else if cell = '?' ∨ cell = c then
            cube := cubeSet cube idx y z c
            x := 0
          else
            cube := cubeSet cube idx y z '.'
            x := idx
  -- top view
  for z in [0:n] do
    let row := topv[z]!
    let zz := n - 1 - z
    for x in [0:n] do
      let c := row[x]!
      if c = '.' then
        for y in [0:n] do
          cube := cubeSet cube x y zz '.'
      else
        let mut y := 0
        while y < n do
          let cell := cubeGet cube x y zz
          if cell = '.' then
            y := y + 1
          else if cell = '?' ∨ cell = c then
            cube := cubeSet cube x y zz c
            y := n
          else
            cube := cubeSet cube x y zz '.'
            y := y + 1
  -- bottom view
  for z in [0:n] do
    let row := bottom[z]!
    let zz := z
    for x in [0:n] do
      let c := row[x]!
      if c = '.' then
        for y in [0:n] do
          cube := cubeSet cube x (n - 1 - y) zz '.'
      else
        let mut y := n
        while y > 0 do
          let idx := y - 1
          let cell := cubeGet cube x idx zz
          if cell = '.' then
            y := idx
          else if cell = '?' ∨ cell = c then
            cube := cubeSet cube x idx zz c
            y := 0
          else
            cube := cubeSet cube x idx zz '.'
            y := idx
  -- count remaining blocks
  let mut cnt := 0
  for x in [0:n] do
    for y in [0:n] do
      for z in [0:n] do
        if cube[x]![y]![z]! ≠ '.' then
          cnt := cnt + 1
  return cnt

/-- Read all tokens from stdin. --/
def readTokens : IO (Array String) := do
  let h ← IO.getStdin
  let s ← h.readToEnd
  let parts := s.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r')
  return parts.filter (· ≠ "").toArray

/-- Entry point. --/
def main : IO Unit := do
  let toks ← readTokens
  if toks.size = 0 then return
  let t := toks[0]!.toNat!
  let mut idx := 1
  for _ in [0:t] do
    let n := toks[idx]!.toNat!
    idx := idx + 1
    let mut front : Array (Array Char) := #[]
    let mut leftv : Array (Array Char) := #[]
    let mut back : Array (Array Char) := #[]
    let mut rightv : Array (Array Char) := #[]
    let mut topv : Array (Array Char) := #[]
    let mut bottom : Array (Array Char) := #[]
    for _ in [0:n] do
      front := front.push (toks[idx]!.data.toArray); idx := idx + 1
      leftv := leftv.push (toks[idx]!.data.toArray); idx := idx + 1
      back := back.push (toks[idx]!.data.toArray); idx := idx + 1
      rightv := rightv.push (toks[idx]!.data.toArray); idx := idx + 1
      topv := topv.push (toks[idx]!.data.toArray); idx := idx + 1
      bottom := bottom.push (toks[idx]!.data.toArray); idx := idx + 1
    let ans := solveCase n front leftv back rightv topv bottom
    IO.println ans
