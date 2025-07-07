let nums = [1; 2; 3]
let letters = ["A"; "B"]

let pairs =
    [ for n in nums do
        for l in letters do
            if n % 2 = 0 then
                yield {| n = n; l = l |} ]

printfn "--- Even pairs ---"
for p in pairs do
    printfn "%d %s" p.n p.l

