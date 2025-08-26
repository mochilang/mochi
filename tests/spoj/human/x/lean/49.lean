/- Solution for SPOJ HAREFOX - Hares and Foxes
https://www.spoj.com/problems/HAREFOX/
-/

import Std
open Std

private def parseFloat (s : String) : Float :=
  let neg := s.startsWith "-"
  let s := if neg then s.drop 1 else s
  let parts := s.splitOn "."
  let intPart := (parts[0]!).foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0
  let fracPart : Float :=
    if parts.length = 2 then
      let fracStr := parts[1]!
      let rec loop (cs : List Char) (place acc : Float) : Float :=
        match cs with
        | []      => acc
        | c :: cs =>
          let d := (c.toNat - '0'.toNat).toFloat
          loop cs (place / 10.0) (acc + d * place)
      loop fracStr.data 0.1 0.0
    else 0.0
  let res := (intPart.toFloat) + fracPart
  if neg then -res else res

private def classify (a b c d h0 f0 : Float) : String :=
  let finish (sH sF : Int) : String :=
    if sH = 2 ∨ sF = 2 then "Chaos will develop."
    else if sH = 0 ∧ sF = 0 then "Ecological balance will develop."
    else if sH = 0 ∨ sF = 0 then "Chaos will develop."
    else if sH > 0 ∧ sF > 0 then "Both hares and foxes will overgrow."
    else if sH < 0 ∧ sF > 0 then "Hares will die out while foxes will overgrow."
    else if sH > 0 ∧ sF < 0 then "Hares will overgrow while foxes will die out."
    else if sH < 0 ∧ sF < 0 then "Both hares and foxes will die out."
    else "Chaos will develop."
  let rec loop (h f : Float) (sH sF : Int) (i : Nat) : String :=
    if Float.abs h < 1e-7 && Float.abs f < 1e-7 then
      "Ecological balance will develop."
    else if i = 0 then
      finish sH sF
    else
      let hn := a * h - b * f
      let fn := c * f + d * h
      let sH' :=
        if Float.abs hn > 1e-3 then
          let s : Int := if hn > 0.0 then 1 else -1
          if sH = 0 then s else if sH ≠ s then 2 else sH
        else sH
      let sF' :=
        if Float.abs fn > 1e-3 then
          let s : Int := if fn > 0.0 then 1 else -1
          if sF = 0 then s else if sF ≠ s then 2 else sF
        else sF
      if Float.abs hn > 1e60 || Float.abs fn > 1e60 then
        finish sH' sF'
      else
        loop hn fn sH' sF' (i-1)
  loop h0 f0 0 0 10000

partial def solveAll (toks : Array String) (idx t : Nat) (acc : List String) : List String :=
  if t = 0 then acc.reverse else
    let a := parseFloat (toks[idx]!)
    let b := parseFloat (toks[idx+1]!)
    let c := parseFloat (toks[idx+2]!)
    let d := parseFloat (toks[idx+3]!)
    let h := parseFloat (toks[idx+4]!)
    let f := parseFloat (toks[idx+5]!)
    let line := classify a b c d h f
    solveAll toks (idx+6) (t-1) (line :: acc)

def main : IO Unit := do
  let h ← IO.getStdin
  let data ← h.readToEnd
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                  |>.filter (fun s => s ≠ "")
                  |> List.toArray
  if toks.size = 0 then
    pure ()
  else
    let t := (toks[0]!).toNat!
    let outs := solveAll toks 1 t []
    for line in outs do
      IO.println line
