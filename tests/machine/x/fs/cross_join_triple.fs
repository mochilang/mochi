open System

type Anon1 = {
    n: int
    l: string
    b: bool
}
let nums: int list = [1; 2]
let letters: string list = ["A"; "B"]
let bools: bool list = [true; false]
let combos: obj list = [ for n in nums do 
  for l in letters do 
  for b in bools do yield { n = n; l = l; b = b } ]
printfn "%s" "--- Cross Join of three lists ---"
for c in combos do
    printfn "%s" (String.concat " " [string c.n; string c.l; string c.b])
