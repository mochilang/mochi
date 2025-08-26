/- Solution for SPOJ CRYPTO1 - The Bytelandian Cryptographer (Act I)
https://www.spoj.com/problems/CRYPTO1
-/
import Std
open Std

namespace Crypto1

-- modular exponentiation
partial def modPow (a b m : Nat) : Nat :=
  let rec loop (a b acc : Nat) : Nat :=
    if b == 0 then acc
    else
      let acc := if b % 2 == 1 then (acc * a) % m else acc
      loop ((a * a) % m) (b / 2) acc
  loop (a % m) b 1

-- check leap year
def isLeap (y : Nat) : Bool :=
  (y % 400 == 0) || ((y % 4 == 0) && (y % 100 != 0))

-- convert seconds since epoch to formatted time string
partial def toDateString (s : Nat) : String :=
  let daySec := 86400
  let totalDays := s / daySec
  let secDay := s % daySec
  let hour := secDay / 3600
  let minute := (secDay % 3600) / 60
  let second := secDay % 60
  let rec yearLoop (y d : Nat) : Nat × Nat :=
    let days := if isLeap y then 366 else 365
    if d >= days then yearLoop (y + 1) (d - days) else (y, d)
  let (year, dayOfYear) := yearLoop 1970 totalDays
  let rec monthLoop (m d : Nat) : Nat × Nat :=
    let dim :=
      match m with
      | 1 => 31
      | 2 => if isLeap year then 29 else 28
      | 3 => 31
      | 4 => 30
      | 5 => 31
      | 6 => 30
      | 7 => 31
      | 8 => 31
      | 9 => 30
      | 10 => 31
      | 11 => 30
      | _ => 31
    if d >= dim then monthLoop (m + 1) (d - dim) else (m, d)
  let (month, day0) := monthLoop 1 dayOfYear
  let day := day0 + 1
  let dow := (totalDays + 4) % 7
  let dayNames := ["Sun","Mon","Tue","Wed","Thu","Fri","Sat"]
  let monNames := ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]
  let dowStr := dayNames.get! dow
  let monStr := monNames.get! (month - 1)
  let dayStr := (if day < 10 then " " else "") ++ toString day
  let hourStr := (if hour < 10 then "0" else "") ++ toString hour
  let minStr := (if minute < 10 then "0" else "") ++ toString minute
  let secStr := (if second < 10 then "0" else "") ++ toString second
  s!"{dowStr} {monStr} {dayStr} {hourStr}:{minStr}:{secStr} {year}"

end Crypto1

open Crypto1

def main : IO Unit := do
  let h ← IO.getStdin
  let line ← h.getLine
  let r := line.trim.toNat!
  let p := 4000000007
  let root := modPow r ((p + 1) / 4) p
  let s := if root ≤ p - root then root else p - root
  IO.println (toDateString s)
