-- https://www.spoj.com/problems/TEST
import Std
open Std

partial def loop (h : IO.FS.Stream) : IO Unit := do
  let line ← h.getLine
  if line == "" then
    return ()
  else
    match line.trim.toNat? with
    | some 42 => return ()
    | some n  => do
        IO.println n
        loop h
    | none => loop h

def main : IO Unit := do
  loop (← IO.getStdin)
