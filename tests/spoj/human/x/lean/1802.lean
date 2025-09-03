/- Solution for SPOJ EDGE - Edge
https://www.spoj.com/problems/EDGE/
-/

import Std
open Std

def solve (s : String) : List String :=
  let rec loop (cs : List Char) (dir : Nat) (x y : Int) (acc : List String) : List String :=
    match cs with
    | [] => acc.reverse
    | c :: rest =>
        let dir := if c = 'A' then (dir + 3) % 4 else (dir + 1) % 4
        let (x, y) :=
          match dir with
          | 0 => (x + 10, y)
          | 1 => (x, y + 10)
          | 2 => (x - 10, y)
          | _ => (x, y - 10)
        loop rest dir x y (s!"{x} {y} lineto" :: acc)
  loop s.data 0 310 420 []

def main : IO Unit := do
  let content ← (← IO.getStdin).readToEnd
  for line in content.trim.splitOn "\n" do
    let s := line.trim
    if s ≠ "" then
      IO.println "300 420 moveto"
      IO.println "310 420 lineto"
      for l in solve s do
        IO.println l
      IO.println "stroke"
      IO.println "showpage"

