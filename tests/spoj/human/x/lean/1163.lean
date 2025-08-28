/- Solution for SPOJ JAVAC - Java vs C++
https://www.spoj.com/problems/JAVAC/
-/

import Std
open Std

private def isJava (s : String) : Bool :=
  match s.data with
  | [] => false
  | c :: cs =>
      c.isLower && cs.all (fun ch => ch.isAlpha && ch != '_')

private def isCpp (s : String) : Bool :=
  let rec loop : List Char -> Bool
    | [] => true
    | '_' :: [] => false
    | '_' :: '_' :: _ => false
    | c :: cs => if c.isUpper then false else loop cs
  match s.data with
  | [] => false
  | '_' :: _ => false
  | l => loop l

private def toCpp (s : String) : String :=
  s.data.foldl (fun acc c =>
    if c.isUpper then
      (acc.push '_').push c.toLower
    else
      acc.push c) ""

private def toJava (s : String) : String :=
  let rec loop : List Char -> String -> Bool -> String
    | [], acc, _ => acc
    | '_' :: cs, acc, _ => loop cs acc true
    | c :: cs, acc, up =>
        let ch := if up then c.toUpper else c
        loop cs (acc.push ch) false
  loop s.data "" false

private def translate (s : String) : String :=
  if isJava s then toCpp s
  else if isCpp s then toJava s
  else "Error!"

partial def process (h : IO.FS.Stream) : IO Unit := do
  if (← h.isEof) then
    pure ()
  else
    let line := (← h.getLine).trim
    if line.isEmpty then
      process h
    else
      IO.println <| translate line
      process h

def main : IO Unit := do
  process (← IO.getStdin)
