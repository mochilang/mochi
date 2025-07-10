open System

type Anon1 = {
    a: int
}
let xs = [1; 2; 3]
let ys = [ for x in xs do if x % 2 = 1 then yield x ]
printfn "%b" (List.contains 1 ys)
printfn "%b" (List.contains 2 ys)
let m = { a = 1 }
printfn "%s" m.ContainsKey "a"
printfn "%s" m.ContainsKey "b"
let s: string = "hello"
printfn "%s" List.contains "ell" s
printfn "%s" List.contains "foo" s
