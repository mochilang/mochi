// Generated 2025-07-21 18:37 +0700

type Anon1 = {
    mutable a: int
}
let xs: int list = [1; 2; 3]
let ys = [ for x in xs do if (x % 2) = 1 then yield x ]
printfn "%b" (List.contains 1 ys)
printfn "%b" (List.contains 2 ys)
let m: Anon1 = { a = 1 }
printfn "%b" (Seq.contains "a" m)
printfn "%b" (Seq.contains "b" m)
let s: string = "hello"
printfn "%b" (s.Contains("ell"))
printfn "%b" (s.Contains("foo"))
