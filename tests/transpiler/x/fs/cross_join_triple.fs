// Generated 2025-07-21 18:37 +0700

type Anon1 = {
    mutable n: obj
    mutable l: obj
    mutable b: obj
}
type Anon2 = {
    mutable n: obj
    mutable l: obj
    mutable b: obj
}
let nums: int list = [1; 2]
let letters: string list = ["A"; "B"]
let bools: bool list = [true; false]
let combos: Anon2 list = [ for n in nums do for l in letters do for b in bools do yield { n = n; l = l; b = b } ]
printfn "%s" (string "--- Cross Join of three lists ---")
for c in combos do
printfn "%s" (String.concat " " [string (c.n); string (c.l); string (c.b)])
