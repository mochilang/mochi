/- Solution for SPOJ QUILT - Little Quilt
https://www.spoj.com/problems/QUILT/
-/

import Std
open Std

structure Quilt where
  rows : List (List Char)

def quiltA : Quilt :=
  { rows := [['\\', '+'], ['\\', '\\']] }

def quiltB : Quilt :=
  { rows := [['|', '|'], ['|', '|']] }

def rotateChar (c : Char) : Char :=
  match c with
  | '-' => '|'
  | '|' => '-'
  | '/' => '\\'
  | '\\' => '/'
  | _ => c

def turn (q : Quilt) : Quilt :=
  let h := q.rows.length
  let w := q.rows.head!.length
  let rows :=
    (List.range w).map (fun r =>
      (List.range h).map (fun c =>
        rotateChar ((q.rows.get! c).get! (w - 1 - r))
      )
    )
  { rows := rows }

def sew? (q1 q2 : Quilt) : Except Unit Quilt :=
  if q1.rows.length = q2.rows.length then
    let rows := List.zipWith (· ++ ·) q1.rows q2.rows
    .ok { rows := rows }
  else
    .error ()

inductive Expr
| A | B | turn (e : Expr) | sew (e1 e2 : Expr)
open Expr

partial def parseExpr : List Char → Except String (Expr × List Char)
| [] => .error "unexpected end"
| 'A' :: rest => .ok (.A, rest)
| 'B' :: rest => .ok (.B, rest)
| 't' :: 'u' :: 'r' :: 'n' :: '(' :: rest => do
    let (e, rest1) ← parseExpr rest
    match rest1 with
    | ')' :: rest2 => .ok (.turn e, rest2)
    | _ => .error "expected )"
| 's' :: 'e' :: 'w' :: '(' :: rest => do
    let (e1, rest1) ← parseExpr rest
    match rest1 with
    | ',' :: rest2 =>
      let (e2, rest3) ← parseExpr rest2
      match rest3 with
      | ')' :: rest4 => .ok (.sew e1 e2, rest4)
      | _ => .error "expected )"
    | _ => .error "expected ,"
| _ => .error "invalid token"

partial def parseAll (cs : List Char) (acc : List Expr := []) : Except String (List Expr) :=
  if cs.isEmpty then
    .ok acc.reverse
  else do
    let (e, rest) ← parseExpr cs
    match rest with
    | ';' :: rest2 => parseAll rest2 (e :: acc)
    | _ => .error "expected ;"

def eval : Expr → Except Unit Quilt
| .A => .ok quiltA
| .B => .ok quiltB
| .turn e => do
    let q ← eval e
    return turn q
| .sew e1 e2 => do
    let q1 ← eval e1
    let q2 ← eval e2
    sew? q1 q2

def printQuilt (q : Quilt) : IO Unit := do
  for row in q.rows do
    IO.println (String.mk row.toArray)

def main : IO Unit := do
  let input ← IO.getStdin.readToEnd
  let chars := input.toList.filter (!Char.isWhitespace ·)
  match parseAll chars with
  | .error _ => pure ()
  | .ok exprs =>
      for (e, idx) in exprs.enum do
        IO.println s!"Quilt {idx+1}:"
        match eval e with
        | .ok q => printQuilt q
        | .error _ => IO.println "error"
