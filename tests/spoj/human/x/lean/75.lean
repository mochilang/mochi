/- Solution for SPOJ EDIT1 - Editor
https://www.spoj.com/problems/EDIT1/
-/

import Std
open Std

/-- process a single line according to editor rules --/
def processLine (s : String) : String :=
  s.foldl (fun acc c => if c == 'd' then acc ++ acc else acc.push c) ""

/-- repeatedly read lines until an empty one and output processed versions --/
partial def loop (h : IO.FS.Stream) : IO Unit := do
  let line := (← h.getLine).trim
  if line.isEmpty then
    pure ()
  else
    IO.println (processLine line)
    loop h

def main : IO Unit := do
  let h ← IO.getStdin
  loop h
