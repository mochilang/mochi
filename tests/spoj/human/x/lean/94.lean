/- Solution for SPOJ MAYA - Numeral System of the Maya
https://www.spoj.com/problems/MAYA/
-/
import Std
open Std

-- Parse a single Maya digit represented with dots (.) and bars (-)
-- segments are separated by spaces from top to bottom
def parseDigit (s : String) : Nat :=
  if s == "S" then 0
  else
    let parts := s.splitOn " "
    let (dots, bars) := parts.foldl
      (fun (db : Nat × Nat) part =>
        let (d, b) := db
        if part == "" then (d, b)
        else
          let c := part.get! 0
          if c = '.' then (d + part.length, b)
          else if c = '-' then (d, b + 1)
          else (d, b))
      (0, 0)
    dots + 5 * bars

-- Convert list of digits (most significant first) to decimal value

def mayaValue (digits : List Nat) : Nat :=
  let rec aux (ds : List Nat) (idx w acc : Nat) : Nat :=
    match ds with
    | [] => acc
    | d :: rest =>
        let acc := acc + d * w
        let w :=
          if idx = 0 then 20
          else if idx = 1 then 360
          else w * 20
        aux rest (idx + 1) w acc
  aux digits.reverse 0 1 0

partial def readDigits (h : IO.FS.Stream) (k : Nat) (acc : List Nat := []) : IO (List Nat) := do
  if k = 0 then
    pure acc.reverse
  else
    let line ← h.getLine
    readDigits h (k - 1) (parseDigit line.trim :: acc)

partial def loop (h : IO.FS.Stream) : IO Unit := do
  let line ← h.getLine
  let line := line.trim
  if line == "" then
    loop h
  else
    let n := line.toNat!
    if n = 0 then
      pure ()
    else
      let digits ← readDigits h n
      IO.println (mayaValue digits)
      loop h

def main : IO Unit := do
  let h ← IO.getStdin
  loop h
