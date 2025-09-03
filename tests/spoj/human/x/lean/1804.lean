/- Solution for SPOJ GENETIC - Genetic Code
https://www.spoj.com/problems/GENETIC/
-/

import Std
open Std

-- expand one digit according to the morphism
def expand (c : Char) : List Char :=
  match c with
  | '0' => ['0', '1', '2']
  | '1' => ['0', '2']
  | _   => ['1']

-- apply the morphism to a whole word
def grow (s : List Char) : List Char :=
  s.foldr (fun c acc => (expand c) ++ acc) []

-- generate a word of length at least n using repeated growth
partial def generate (n : Nat) (s : List Char := ['0']) : List Char :=
  if s.length >= n then s
  else generate n (grow s)

-- map digits to genome letters
def thue (n : Nat) : String :=
  let digits := (generate n).take n
  let letters := digits.map (fun c =>
    match c with
    | '0' => 'N'
    | '1' => 'O'
    | _   => 'P')
  String.mk letters

partial def loop (h : IO.FS.Stream) : IO Unit := do
  let line ← h.getLine
  let n := line.trim.toNat!
  if n == 0 then
    pure ()
  else
    IO.println (thue n)
    loop h

def main : IO Unit := do
  loop (← IO.getStdin)
