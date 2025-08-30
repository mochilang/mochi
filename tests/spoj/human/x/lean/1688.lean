-- https://www.spoj.com/problems/EASYPROB/

def pow2 (k : Nat) : Nat := Nat.pow 2 k

partial def highestPow (n : Nat) : Nat :=
  let rec aux (k : Nat) : Nat :=
    if pow2 (k + 1) > n then k else aux (k + 1)
  aux 0

partial def reprExp : Nat -> String
| 0 => "0"
| n =>
  let k := highestPow n
  let term :=
    if k = 0 then "2(0)"
    else if k = 1 then "2"
    else "2(" ++ reprExp k ++ ")"
  let rest := n - pow2 k
  if rest = 0 then term else term ++ "+" ++ reprExp rest

def format (n : Nat) : String :=
  s!"{n}={reprExp n}"

def main : IO Unit := do
  let nums := [137, 1315, 73, 136, 255, 1384, 16385]
  for n in nums do
    IO.println (format n)
