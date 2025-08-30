/- Solution for SPOJ BLUEEQ - Help Blue Mary Please! (Act I)
https://www.spoj.com/problems/BLUEEQ/
-/

import Std
open Std

-- Convert letter 'a'..'m' to index 0..12
def idx (c : Char) : Nat := c.toNat - 'a'.toNat

-- Check if character is digit
def isDigit (c : Char) : Bool := c ≥ '0' && c ≤ '9'

def charVal (c : Char) : Nat := c.toNat - '0'.toNat

partial def readDigits (acc : Nat) : List Char → (Nat × List Char)
  | [] => (acc, [])
  | c :: cs => if isDigit c then readDigits (acc * 10 + charVal c) cs else (acc, c :: cs)

partial def parseNumber : List Char → Option (Nat × List Char)
  | [] => none
  | c :: cs =>
      if !isDigit c then none
      else if c == '0' then
        match cs with
        | d :: ds => if isDigit d then none else some (0, cs)
        | [] => some (0, [])
      else
        let (v, rest) := readDigits (charVal c) cs
        some (v, rest)

partial def parseProd : List Char → Option (Nat × List Char)
  | cs =>
      match parseNumber cs with
      | none => none
      | some (v, rest) =>
          let rec loop (acc : Nat) : List Char → Option (Nat × List Char)
            | '*' :: cs2 =>
                match parseNumber cs2 with
                | some (n, rest2) => loop (acc * n) rest2
                | none => none
            | xs => some (acc, xs)
          loop v rest

partial def parseSum : List Char → Option (Nat × List Char)
  | cs =>
      match parseProd cs with
      | none => none
      | some (v, rest) =>
          let rec loop (acc : Nat) : List Char → Option (Nat × List Char)
            | '+' :: cs2 =>
                match parseProd cs2 with
                | some (n, rest2) => loop (acc + n) rest2
                | none => none
            | xs => some (acc, xs)
          loop v rest

def evalExpr (s : String) : Option Nat :=
  match parseSum s.data with
  | some (v, []) => some v
  | _ => none

-- Check equation validity under mapping
def checkEq (mapping : Array Char) (eqs : Array String) : Bool :=
  eqs.all (fun s =>
    let mapped := s.data.map (fun c => mapping.get! (idx c))
    let str := String.mk mapped
    let parts := str.splitOn "="
    if parts.length ≠ 2 then false
    else match evalExpr parts[0]!, evalExpr parts[1]! with
      | some a, some b => a == b
      | _, _ => false)

-- DFS search for mappings
partial def dfs (letters : List Nat) (mapping : Array Char) (used : Array Bool)
    (eqs : Array String) : List (Array Char) :=
  match letters with
  | [] => if checkEq mapping eqs then [mapping] else []
  | i :: rest =>
      let syms : List Char :=
        ['0','1','2','3','4','5','6','7','8','9','+','*']
      let rec loopSyms : List Char → List (Array Char)
        | [] => []
        | s :: ss =>
            let si :=
              if isDigit s then charVal s
              else if s == '+' then 10
              else 11
            if used.get! si then
              loopSyms ss
            else
              let mapping' := mapping.set! i s
              let used' := used.set! si true
              dfs rest mapping' used' eqs ++ loopSyms ss
      loopSyms syms

-- Solve a test case
def solveCase (eqs : Array String) : List String :=
  let mut present : Array Bool := Array.mkArray 13 false
  for s in eqs do
    for c in s.data do
      present := present.set! (idx c) true
  -- candidates for '='
  let mut cand : List Nat := List.range 13
  for s in eqs do
    let mut counts : Array Nat := Array.mkArray 13 0
    for c in s.data do
      counts := counts.modify (idx c) (·+1)
    cand := cand.filter (fun i => counts.get! i == 1)
  let mut solutions : List (Array Char) := []
  for e in cand do
    let mut mapping : Array Char := Array.mkArray 13 ' '
    mapping := mapping.set! e '='
    let used : Array Bool := Array.mkArray 12 false
    let mut letters : List Nat := []
    for i in [0:13] do
      if present.get! i && i ≠ e then
        letters := i :: letters
    let sols := dfs letters mapping used eqs
    solutions := sols ++ solutions
  if solutions = [] then
    ["noway"]
  else
    let mut poss : Array (Std.HashSet Char) := Array.mkArray 13 {}
    for m in solutions do
      for i in [0:13] do
        let ch := m.get! i
        if present.get! i && ch ≠ ' ' then
          poss := poss.modify i (·.insert ch)
    let mut res : List String := []
    for i in [0:13] do
      if present.get! i then
        let set := poss.get! i
        if set.size == 1 then
          let sym := set.toList.head!
          res := res ++ [String.mk [Char.ofNat (i + 'a'.toNat), sym]]
    res

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  let mut outs : List String := []
  for _ in [0:t] do
    let n := (← h.getLine).trim.toNat!
    let mut arr : Array String := #[]
    for _ in [0:n] do
      arr := arr.push (← h.getLine).trim
    let res := solveCase arr
    outs := outs ++ [String.intercalate "\n" res]
  IO.println (String.intercalate "\n" outs)
