/- Solution for SPOJ STEVE - Voracious Steve
https://www.spoj.com/problems/STEVE/
-/
import Std
open Std

partial def steve (n m : Nat) : Nat :=
  Id.run do
    let mut dp := Array.mkArray (n + 1) 0
    for i in [1:n+1] do
      let q := i / (m + 1)
      let r := i % (m + 1)
      let maxf :=
        if q == 0 then
          0
        else
          Id.run do
            let mut best := 0
            for k in [q:(q * m) + 1] do
              let v := dp.get! k
              if v > best then
                best := v
            pure best
      if r > 0 then
        dp := dp.set! i (i - maxf)
      else
        dp := dp.set! i maxf
    pure (dp.get! n)

partial def readCases (toks : Array String) (idx t : Nat) (acc : List String) : List String :=
  if t = 0 then
    acc.reverse
  else
    let n := toks.get! idx |>.toNat!
    let m := toks.get! (idx+1) |>.toNat!
    let res := steve n m
    readCases toks (idx+2) (t-1) (toString res :: acc)

def main : IO Unit := do
  let data ← IO.readStdin
  let toks := data.split (fun c => c = ' ' ∨ c = '\n' ∨ c = '\t' ∨ c = '\r')
                |>.filter (fun s => s ≠ "")
                |> Array.ofList
  let t := toks.get! 0 |>.toNat!
  let outs := readCases toks 1 t []
  for line in outs do
    IO.println line
