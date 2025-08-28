/- Solution for SPOJ TFSETS - Triple-Free Sets
https://www.spoj.com/problems/TFSETS/
-/

import Std
open Std

def MOD : Nat := 1000000001

def limits : List Nat := [1,2,3,4,6,8,9,12,16,18,24,27,32,36,48,54,64,72,81,96,108,128,144,162,192,216,243,256,288,324,384,432,486,512,576,648,729,768,864,972,1024,1152,1296,1458,1536,1728,1944,2048,2187,2304,2592,2916,3072,3456,3888,4096,4374,4608,5184,5832,6144,6561,6912,7776,8192,8748,9216,10368,11664,12288,13122,13824,15552,16384,17496,18432,19683,20736,23328,24576,26244,27648,31104,32768,34992,36864,39366,41472,46656,49152,52488,55296,59049,62208,65536,69984,73728,78732,82944,93312,98304]

def gVals : List Nat := [2,3,5,8,12,19,33,49,78,114,172,294,467,703,1055,1559,2478,3718,6394,9616,14366,22827,34363,50543,75935,114316,196149,311714,468492,702436,1055706,1585526,2336786,3713369,5584289,8390475,14409962,21653776,32557779,48738745,77452142,116437300,175099856,257850206,387496684,582226892,875754843,391672638,389191466,592286425,398891564,87893797,154145863,266640053,452360683,625056365,264516690,619865276,238569609,345745088,124243652,403748297,646714199,905024632,793465262,308848199,605636282,290598246,46709814,38105878,281874215,760246709,93099279,41593891,790530768,692325370,784426746,185693996,77303869,897362654,929177580,380774435,630418830,127649001,920357089,334124113,358039273,43598907,649146845,542555108,576282892,289862284,385341741,147796640,960758401,834024535,396043521,732355928,802195667,699887506,570710481]

def gQuery (k : Nat) : Nat :=
  let rec go (ls vs : List Nat) (last : Nat) :=
    match ls, vs with
    | l :: ls', v :: vs' =>
        if k < l then last else go ls' vs' v
    | _, _ => last
  go limits gVals 1

partial def powMod (a b : Nat) : Nat :=
  if b == 0 then 1 % MOD
  else
    let h := powMod a (b / 2)
    let hh := (h * h) % MOD
    if b % 2 == 0 then hh else (hh * (a % MOD)) % MOD

def countCoprime6 (l r : Nat) : Nat :=
  let total := r - l + 1
  let by2 := r / 2 - (l - 1) / 2
  let by3 := r / 3 - (l - 1) / 3
  let by6 := r / 6 - (l - 1) / 6
  total - by2 - by3 + by6

partial def Floop (n i ans : Nat) : Nat :=
  if i > n then ans
  else
    let k := n / i
    let j := n / k
    let c := countCoprime6 i j
    let ans := (ans * powMod (gQuery k) c) % MOD
    Floop n (j + 1) ans

def F (n : Nat) : Nat := Floop n 1 1

partial def process (h : IO.FS.Stream) (t : Nat) : IO Unit := do
  if t == 0 then pure ()
  else
    let line ← h.getLine
    let n := line.trim.toNat!
    IO.println (F n)
    process h (t - 1)

def main : IO Unit := do
  let h ← IO.getStdin
  let tLine ← h.getLine
  process h (tLine.trim.toNat!)
