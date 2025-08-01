// Generated 2025-07-21 18:37 +0700

type Anon1 = {
    n: obj
    l: obj
    b: obj
}


type Anon2 = {
    n: obj
    l: obj
    b: obj
}


let nums: int list = [1; 2]

let letters: string list = ["A"; "B"]

let bools = bool list

let combos: Anon2 list = [for n in nums do
for l in letters do
for b in bools do



]

printfn "%s" (string "--- Cross Join of three lists ---")

c

combos

printfn "%s"

(String.concat " " [string (c.n); string (c.l); string (c.b)])
