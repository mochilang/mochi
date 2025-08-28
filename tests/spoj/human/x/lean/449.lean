-- Solution for SPOJ TCNUMFL - Simple Numbers with Fractions Conversion
-- https://www.spoj.com/problems/TCNUMFL
import Std
open Std

-- convert character to numeric value
def charToDigit (c : Char) : Nat :=
  let n := c.toNat
  let zero := '0'.toNat
  let nine := '9'.toNat
  if n <= nine then n - zero else n - 'A'.toNat + 10

-- parse a string in base b to a Nat
def parseBase (s : String) (b : Nat) : Nat :=
  s.data.foldl (fun acc ch => acc * b + charToDigit ch) 0

-- convert a Nat to digits in base b (as list of chars)
def toDigits (n b : Nat) : List Char :=
  let rec loop (x : Nat) (acc : List Char) : List Char :=
    if x = 0 then acc
    else
      let d := x % b
      let c := if d < 10 then Char.ofNat (d + '0'.toNat)
               else Char.ofNat (d - 10 + 'A'.toNat)
      loop (x / b) (c :: acc)
  if n = 0 then ['0'] else loop n []

def toDigitsPad (n b len : Nat) : String :=
  if len = 0 then ""
  else
    let digits := toDigits n b
    let pad := if digits.length < len then List.replicate (len - digits.length) '0' else []
    String.mk (pad ++ digits)

def convert (num : String) (r s l : Nat) : String :=
  let parts := num.splitOn ","
  let intPart := parts.get! 0
  let fracPart := (parts.get? 1).getD ""
  let intVal := parseBase intPart r
  let k := fracPart.length
  let fracVal := parseBase fracPart r
  let numerator := intVal * r^k + fracVal
  let denom := r^k
  let spow := s^l
  let scaled := (numerator * spow) / denom
  let intOut := scaled / spow
  let fracOut := scaled % spow
  let intStr := String.mk (toDigits intOut s)
  let fracStr := toDigitsPad fracOut s l
  if l = 0 then intStr else intStr ++ "," ++ fracStr

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t = 0 then
    pure ()
  else
    let line ← h.getLine
    let parts := line.trim.splitOn " "
    let nstr := parts.get! 0
    let r := parts.get! 1 |>.toNat!
    let s := parts.get! 2 |>.toNat!
    let l := parts.get! 3 |>.toNat!
    IO.println (convert nstr r s l)
    process h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let t := (← h.getLine).trim.toNat!
  process h t
