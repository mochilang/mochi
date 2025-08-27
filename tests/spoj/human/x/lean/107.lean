/- Solution for SPOJ MAYACAL - Calendar of the Maya
https://www.spoj.com/problems/MAYACAL/
-/

import Std
open Std

/-- Tzolkin day names in order -/
def tzNames : Array String := #["Imix", "Ik", "Akbal", "Kan", "Chikchan", "Kimi", "Manik", "Lamat", "Muluk", "Ok", "Chuen", "Eb", "Ben", "Ix", "Men", "Kib", "Kaban", "Etznab", "Kawak", "Ajaw"]

/-- Haab month names and their lengths -/
def haabNames : Array String := #["Pohp", "Wo", "Sip", "Zotz", "Sek", "Xul", "Yaxkin", "Mol", "Chen", "Yax", "Sak", "Keh", "Mak", "Kankin", "Muan", "Pax", "Kayab", "Kumku", "Wayeb"]

def haabDays : Array Nat := #[20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,5]

/-- find index of a string in an array -/
def findIdx (arr : Array String) (s : String) : Option Nat :=
  arr.findIdx? (· = s)

/-- compute Tzolkin number/name and Haab day/month for x days after 8.0.0.0.0 -/
def computeCR (x : Nat) : (Nat × Nat × Nat × Nat) :=
  let tzNum := ((8 + x) % 13) + 1
  let tzIdx := (19 + x) % 20
  let haabIdx := (42 + x) % 365
  let hMonth := haabIdx / 20
  let hDay := if hMonth == 18 then (haabIdx - 360) + 1 else (haabIdx % 20) + 1
  (tzNum, tzIdx, hDay, hMonth)

/-- convert absolute day count to Long Count components -/
def toLongCount (absDay : Nat) : (Nat × Nat × Nat × Nat × Nat) :=
  let b := absDay / 144000
  let rem1 := absDay % 144000
  let k := rem1 / 7200
  let rem2 := rem1 % 7200
  let t := rem2 / 360
  let rem3 := rem2 % 360
  let w := rem3 / 20
  let i := rem3 % 20
  (b,k,t,w,i)

/-- process one Calendar Round date line -/
def process (line : String) : IO Unit :=
  let parts := line.trim.split (· = ' ') |>.filter (· ≠ "")
  if parts.length != 4 then
    IO.println "0"
  else
    let tzNum := parts[0]!.toNat!
    let tzName := parts[1]!
    let haabDay := parts[2]!.toNat!
    let haabMonth := parts[3]!
    match findIdx tzNames tzName, findIdx haabNames haabMonth with
    | some tzIdx, some hMonth => do
        let maxDay := haabDays[hMonth]!
        if tzNum == 0 || tzNum > 13 || haabDay == 0 || haabDay > maxDay then
          IO.println "0"
        else
          let mut res : Array (Nat × Nat × Nat × Nat × Nat) := #[]
          for x in [0:288000] do
            let (tn, ti, hd, hm) := computeCR x
            if tzNum == tn && tzIdx == ti && haabDay == hd && hMonth == hm then
              let lc := toLongCount (8*144000 + x)
              res := res.push lc
          if res.isEmpty then
            IO.println "0"
          else
            IO.println res.size
            for (b,k,t,w,i) in res do
              IO.println s!"{b}.{k}.{t}.{w}.{i}"
    | _, _ => IO.println "0"

/-- main entry point -/
def main : IO Unit := do
  let h ← IO.getStdin
  let d := (← h.getLine).trim.toNat!
  for _ in [0:d] do
    let line ← h.getLine
    process line
