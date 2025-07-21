// Generated 2025-07-21 18:37 +0700

type Anon1 = {
    mutable n: obj
    mutable l: obj
}
type Anon2 = {
    mutable n: obj
    mutable l: obj
}
let nums: int list = [1; 2; 3]
let letters: string list = ["A"; "B"]
let pairs: Anon2 list = [ for n in nums do for l in letters do if (n % 2) = 0 then yield { n = n; l = l } ]
printfn "%s" (string "--- Even pairs ---")
for p in pairs do
printfn "%s" (String.concat " " [string (p.n); string (p.l)])
