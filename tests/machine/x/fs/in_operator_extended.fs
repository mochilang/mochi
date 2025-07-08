open System

exception Break
exception Continue

let xs = [1; 2; 3]
let ys = [ for x in xs do if x % 2 = 1 then yield x ]
printfn "%A" (List.contains 1 ys)
printfn "%A" (List.contains 2 ys)
let m = dict [(a, 1)]
printfn "%A" (List.contains "a" m)
printfn "%A" (List.contains "b" m)
let s = "hello"
printfn "%A" (List.contains "ell" s)
printfn "%A" (List.contains "foo" s)
