/- Solution for SPOJ SCALES - Balancing the Stone
https://www.spoj.com/problems/SCALES/
-/

import Std
open Std

/-- Parse a binary string into an array of bits (least significant first). -/
partial def parseBits (s : String) : Array Nat :=
  let rec build : List Char → Array Nat → Array Nat
  | [], acc => acc
  | c :: cs, acc => build cs (acc.push (if c = '1' then 1 else 0))
  build s.data.reverse #[]

/-- Count the number of ways to balance the stone. -/
partial def countWays (n : Nat) (bits : Array Nat) (modulus : Nat) : Nat :=
  let rec loop (i : Nat) (dn dz dp : Nat) : Nat :=
    if i < n then
      let bit := if i < bits.size then bits[i]! else 0
      if bit = 0 then
        loop (i + 1) (dn % modulus)
          ((dn + dz + dp) % modulus)
          (dp % modulus)
      else
        loop (i + 1)
          ((dn + dz) % modulus)
          ((dz + dp) % modulus)
          0
    else
      dz % modulus
  loop 0 0 1 0

/-- Process each test case. -/
partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let line ← h.getLine
    if line.trim.isEmpty then
      process h t
    else
      let parts := line.trim.splitOn " "
      let n := (parts[0]!).toNat!
      let _l := (parts[1]!).toNat!
      let d := (parts[2]!).toNat!
      let w ← h.getLine
      let bits := parseBits w.trim
      let ans := countWays n bits d
      IO.println ans
      process h (t - 1)

/-- Entry point. -/
def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
