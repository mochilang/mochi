/- Solution for SPOJ OFORTUNE - Ohgas' Fortune
https://www.spoj.com/problems/OFORTUNE/
-/
import Std
open Std

/-- simulate one operation and return final amount --/
def calc (fund years : Nat) (isComp : Bool) (rateUnits charge : Nat) : Nat :=
  let mut bal := fund
  let mut gain := 0
  for _ in [0:years] do
    let interest := bal * rateUnits / 8192
    if isComp then
      bal := bal + interest - charge
    else
      gain := gain + interest
      bal := bal - charge
  if isComp then bal else bal + gain

partial def solve (arr : Array String) (idx m : Nat) (out : Array String) : Array String :=
  if m = 0 then out
  else
    let fund := (arr.get! idx).toNat!
    let years := (arr.get! (idx+1)).toNat!
    let ops := (arr.get! (idx+2)).toNat!
    let mut j := idx + 3
    let mut best : Nat := 0
    for _ in [0:ops] do
      let kind := arr.get! j
      let rateStr := arr.get! (j+1)
      let charge := (arr.get! (j+2)).toNat!
      let rUnits := (Float.floor ((rateStr.toFloat!) * 8192.0)).toUInt64.toNat
      let res := calc fund years (kind = "1") rUnits charge
      if res > best then
        best := res
      j := j + 3
    let out' := out.push (toString best)
    solve arr j (m-1) out'

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' || c = '\n' || c = '\t' || c = '\r') |>.filter (· ≠ "")
  let arr := Array.ofList toks
  if arr.size = 0 then return ()
  let m := (arr.get! 0).toNat!
  let results := solve arr 1 m #[]
  for line in results do
    IO.println line
