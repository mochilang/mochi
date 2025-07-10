
type Anon1 = {
    n: obj
    l: obj
    b: obj
}
let nums = [1; 2]
let letters = ["A"; "B"]
let bools = [true; false]
let combos = [ for n in nums do 
  for l in letters do 
  for b in bools do yield { n = n; l = l; b = b } ]
printfn "%s" "--- Cross Join of three lists ---"
for c in combos do
    printfn "%s" (String.concat " " [string c.n; string c.l; string c.b])
