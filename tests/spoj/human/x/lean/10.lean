/- Solution for SPOJ CMEXPR - Complicated Expressions
https://www.spoj.com/problems/CMEXPR/
-/

import Std
open Std

inductive Expr where
  | var : Char -> Expr
  | op  : Expr -> Char -> Expr -> Expr
deriving Inhabited

def prec : Char -> Nat
  | '+' | '-' => 1
  | '*' | '/' => 2
  | _         => 3

mutual
  partial def parseExpr (cs : List Char) : Expr × List Char :=
    let (t, rest) := parseTerm cs
    parseExprLoop t rest
  partial def parseExprLoop (acc : Expr) (cs : List Char) : Expr × List Char :=
    match cs with
    | '+' :: rest =>
        let (t, r) := parseTerm rest
        parseExprLoop (.op acc '+' t) r
    | '-' :: rest =>
        let (t, r) := parseTerm rest
        parseExprLoop (.op acc '-' t) r
    | _ => (acc, cs)

  partial def parseTerm (cs : List Char) : Expr × List Char :=
    let (f, rest) := parseFactor cs
    parseTermLoop f rest
  partial def parseTermLoop (acc : Expr) (cs : List Char) : Expr × List Char :=
    match cs with
    | '*' :: rest =>
        let (f, r) := parseFactor rest
        parseTermLoop (.op acc '*' f) r
    | '/' :: rest =>
        let (f, r) := parseFactor rest
        parseTermLoop (.op acc '/' f) r
    | _ => (acc, cs)

  partial def parseFactor : List Char -> Expr × List Char
    | '(' :: rest =>
        let (e, r) := parseExpr rest
        match r with
        | ')' :: r2 => (e, r2)
        | _         => (e, r)
    | c :: rest => (.var c, rest)
    | [] => (.var 'x', [])
end

def parse (s : String) : Expr :=
  (parseExpr s.toList).1

partial def needParen (p : Char) (child : Expr) (isLeft : Bool) : Bool :=
  match child with
  | .var _ => False
  | .op _ q _ =>
      let pp := prec p
      let pq := prec q
      if pq < pp then True
      else if pq > pp then False
      else if isLeft then False
      else
        match p with
        | '-' => q = '+' || q = '-'
        | '/' => True
        | _   => False

mutual
  partial def toStringExpr : Expr -> String
    | .var c     => String.singleton c
    | .op l op r =>
        let ls := childStr l op True
        let rs := childStr r op False
        ls ++ String.singleton op ++ rs

  partial def childStr (e : Expr) (p : Char) (isLeft : Bool) : String :=
    let s := toStringExpr e
    if needParen p e isLeft then "(" ++ s ++ ")" else s
end

partial def simplify (s : String) : String :=
  toStringExpr (parse s)

partial def loop (h : IO.FS.Stream) (n : Nat) : IO Unit := do
  if n = 0 then
    pure ()
  else
    let line ← h.getLine
    IO.println <| simplify line.trim
    loop h (n-1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  loop h t
