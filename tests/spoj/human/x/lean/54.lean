/- Solution for SPOJ JULKA - Julka
https://www.spoj.com/problems/JULKA/
-/

import Std
open Std

partial def loop (h : IO.FS.Stream) : IO Unit := do
  try
    let total := (← h.getLine).trim.toNat!
    let diff := (← h.getLine).trim.toNat!
    let klaudia := (total + diff) / 2
    let natalia := (total - diff) / 2
    IO.println klaudia
    IO.println natalia
    loop h
  catch _ =>
    pure ()

def main : IO Unit := do
  loop (← IO.getStdin)
