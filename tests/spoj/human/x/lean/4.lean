/- Solution for SPOJ ONP - Transform the Expression
https://www.spoj.com/problems/ONP/
-/

import Std
open Std

def precedence (c : Char) : Nat :=
  match c with
  | '+' | '-' => 1
  | '*' | '/' => 2
  | '^'       => 3
  | _         => 0

def isLetter (c : Char) : Bool :=
  let n := c.toNat
  decide ('a'.toNat ≤ n) && decide (n ≤ 'z'.toNat)

partial def toRPN (expr : String) : String :=
  let rec process (chars : List Char) (stack : List Char) (out : List Char) : String :=
    match chars with
    | [] =>
      let rec drain (s : List Char) (o : List Char) : List Char :=
        match s with
        | [] => o
        | '(' :: rest => drain rest o
        | op :: rest => drain rest (op :: o)
      String.mk <| List.reverse (drain stack out)
    | ch :: rest =>
      if isLetter ch then
        process rest stack (ch :: out)
      else if ch == '(' then
        process rest (ch :: stack) out
      else if ch == ')' then
        let rec popUntil (s : List Char) (o : List Char) : (List Char × List Char) :=
          match s with
          | [] => ([], o)
          | '(' :: tail => (tail, o)
          | op :: tail => popUntil tail (op :: o)
        let (stack', out') := popUntil stack out
        process rest stack' out'
      else
        let rec popOps (s : List Char) (o : List Char) : (List Char × List Char) :=
          match s with
          | [] => ([], o)
          | '(' :: _ => (s, o)
          | op :: tail =>
            if precedence op > precedence ch ∨ (precedence op = precedence ch ∧ ch ≠ '^') then
              popOps tail (op :: o)
            else
              (s, o)
        let (stack', out') := popOps stack out
        process rest (ch :: stack') out'
  process expr.data [] []

partial def handle (h : IO.FS.Stream) : IO Unit := do
  let tLine ← h.getLine
  let mut t := tLine.trim.toNat!
  for _ in List.range t do
    let line ← h.getLine
    IO.println (toRPN line.trim)

def main : IO Unit := do
  handle (← IO.getStdin)
