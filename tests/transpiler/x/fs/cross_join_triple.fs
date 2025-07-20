// Generated 2025-07-20 21:29 +0700
open System

type Anon1 = {
    n: obj
    l: obj
    b: obj
}
let nums = [1; 2]
let letters = ["A"; "B"]
let bools = [true; false]
let combos: Anon1 list = [ for n in nums do for l in letters do for b in bools do yield { n = n; l = l; b = b } ]
printfn "%s" (string "--- Cross Join of three lists ---")
for c in combos do
printfn "%s" (String.concat " " [string (c.n); string (c.l); string (c.b)])
